//
// Created by coder2k on 24.06.2022.
//

#include "emitter.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "namespace.hpp"
#include "parser.hpp"
#include "types.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <optional>
#include <stack>
#include <string_view>

[[nodiscard]] static u8 char_token_to_u8(const Lexer::Tokens::CharLiteral& token) {
    const auto view = token.location.view();
    assert(view.front() == '\'');
    assert(view.back() == '\'');
    const auto is_escaped = (view[1] == '\\');
    assert((is_escaped and view.size() == 4) or (not is_escaped and view.size() == 3));
    if (not is_escaped) {
        return static_cast<u8>(view[1]);
    }
    switch (view[2]) {
        case '\'':
        case '\\':
            return static_cast<u8>(view[2]);
        case 't':
            return static_cast<u8>('\t');
        case 'n':
            return static_cast<u8>('\n');
        case 'v':
            return static_cast<u8>('\v');
        case 'f':
            return static_cast<u8>('\f');
        case 'r':
            return static_cast<u8>('\r');
        case '0':
            return 0;
        default:
            Error::error(token, "invalid escape sequence");
            return 0;
    }
}

namespace Emitter {
    using namespace Parser::Statements;
    using namespace Parser::Expressions;

    struct LoopLabels {
        std::string continue_to_label;
        std::string break_to_label;
    };

    struct EmitterVisitor : public ExpressionVisitor, public StatementVisitor {
        struct BinaryOperatorEmitter {
            void operator()(const Lexer::Tokens::Plus&) const {
                visitor->emit("add R1, R2, R3", "add values");
            }

            void operator()(const Lexer::Tokens::Minus&) const {
                assert(false && "not implemented");
            }

            void operator()(const Lexer::Tokens::Asterisk&) const {
                visitor->emit("mult R1, R2, R4, R3", "multiply values");
            }

            void operator()(const Lexer::Tokens::ForwardSlash&) const {
                assert(false && "not implemented");
            }

            void operator()(const Lexer::Tokens::And&) const {
                visitor->emit("and R1, R2, R3", "and values");
            }

            void operator()(const Lexer::Tokens::Or&) const {
                visitor->emit("or R1, R2, R3", "or values");
            }

            void operator()(const auto&) const {
                assert(false && "unreachable");
            }

            EmitterVisitor* visitor;
        };

        EmitterVisitor(const Parser::Program* program) : program{ program } { }

        std::string assembly;
        const Parser::Program* program;

        void emit(const std::string_view instruction) {
            emit(instruction, "");
        }

        void emit(const std::string_view instruction, const std::string_view comment) {
            assembly += fmt::format("\t{}", instruction);
            if (not comment.empty()) {
                if (not instruction.empty()) {
                    assembly += " ";
                }
                assembly += fmt::format("// {}", comment);
            }
            assembly += "\n";
        }

        void emit_label(const std::string_view label, const std::string_view comment = "") {
            assembly += fmt::format("{}:", label);
            if (not comment.empty()) {
                assembly += fmt::format(" // {}", comment);
            }
            assembly += "\n";
        }

        void visit(Integer& expression) override {
            emit(fmt::format("copy {}, R1", expression.value.location.view()), "put immediate into register");
            emit("push R1", "push immediate onto stack");
        }

        void visit(Char& expression) override {
            emit(fmt::format("copy {}, R1", char_token_to_u8(expression.value)), "put immediate into register");
            emit("push R1", "push immediate onto stack");
        }

        void visit(Bool& expression) override {
            const u8 value = expression.value.location.view() == "true" ? 1 : 0;
            emit(fmt::format("copy {}, R1", value), "put immediate into register");
            emit("push R1", "push immediate onto stack");
        }

        void visit(Name& expression) override {
            assert(expression.data_type && "data type must be known at this point");
            const auto is_function = expression.possible_overloads.has_value();
            if (is_function) {
                assert(expression.possible_overloads.value().size() == 1);
                const auto& overload = expression.possible_overloads.value().front();
                const auto mangled_name = fmt::format("{}{}", overload->namespace_name, overload->signature);
                emit(fmt::format("copy {}, R1", mangled_name), "get address of label");
                emit("push R1", "push address of label onto stack");
                return;
            }

            const auto& variable_token = expression.name_tokens.back();
            const auto variable_name = Error::token_location(variable_token).view();

            if (expression.name_tokens.size() != 1) {
                Error::error(expression.name_tokens.back(), "qualified name not allowed here");
            }
            std::optional<usize> offset;
            const Scope* current_scope = expression.surrounding_scope;
            while (current_scope != nullptr) {
                const auto find_iterator = current_scope->find(variable_name);
                if (find_iterator != current_scope->end()) {
                    offset = std::get<VariableSymbol>(find_iterator->second).offset;
                    break;
                }
                current_scope = current_scope->surrounding_scope;
            }
            if (not offset.has_value()) {
                Error::error(variable_token, fmt::format("use of undeclared identifier \"{}\"", variable_name));
            }
            emit(fmt::format("add R0, {}, R1", offset.value()),
                 fmt::format("calculate address of variable \"{}\"", variable_name));
            emit("copy *R1, R2", fmt::format("load value of variable \"{}\" into R2", variable_name));
            emit("push R2", fmt::format("push value of variable \"{}\" onto the stack", variable_name));
        }

        void visit(BinaryOperator& expression) override {
            using namespace Lexer::Tokens;

            expression.lhs->accept(*this);
            if (is<And>(expression.operator_token)) {
                emit("pop R1", fmt::format(
                                       R"(store lhs for {}-operator in R1)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                const auto end_of_evaluation = next_label("end_of_short_circuiting");
                emit(fmt::format("jump_eq R1, {}", end_of_evaluation), "skip rest of evaluation if value is false");
                expression.rhs->accept(*this);
                emit("pop R2", fmt::format(
                                       R"(store rhs for {}-operator in R2)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                std::visit(BinaryOperatorEmitter{ this }, expression.operator_token);
                const auto after_push = next_label("after_push");
                emit(fmt::format("jump {}", after_push));
                emit_label(end_of_evaluation);
                emit("copy R1, R3", "store \"false\" as result");
                emit_label(after_push);
            } else if (is<Or>(expression.operator_token)) {
                emit("pop R1", fmt::format(
                                       R"(store lhs for {}-operator in R1)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                const auto end_of_evaluation = next_label("end_of_short_circuiting");
                emit(fmt::format("jump_gt R1, {}", end_of_evaluation), "skip rest of evaluation if value is true");
                expression.rhs->accept(*this);
                emit("pop R2", fmt::format(
                                       R"(store rhs for {}-operator in R2)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                std::visit(BinaryOperatorEmitter{ this }, expression.operator_token);
                const auto after_push = next_label("after_push");
                emit(fmt::format("jump {}", after_push));
                emit_label(end_of_evaluation);
                emit("copy R1, R3", "store \"true\" as result");
                emit_label(after_push);
            } else {
                expression.rhs->accept(*this);

                emit("pop R2", fmt::format(
                                       R"(store rhs for {}-operator in R2)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                emit("pop R1", fmt::format(
                                       R"(store lhs for {}-operator in R1)",
                                       Error::token_location(expression.operator_token).view()
                               ));

                std::visit(BinaryOperatorEmitter{ this }, expression.operator_token);
            }
            emit("push R3", "push result onto stack");
        }

        void visit(FunctionCall& expression) override {
            expression.callee->accept(*this);// evaluate callee

            emit("pop R5", "get jump address");

            emit("copy sp, R6", "store address of return address placeholder");
            emit("add sp, 4, sp", "reserve stack space for the return address placeholder");

            emit("push R0", "store current stack frame base pointer for later");
            emit("copy sp, R7", "this will be the new stack frame base pointer");

            emit("", "evaluate arguments in current stack frame");
            for (const auto& argument : expression.arguments) {
                argument->accept(*this);
            }

            emit("copy R7, R0", "set the new stack frame base pointer");

            emit("add ip, 24, R1", "calculate return address");
            emit("copy R1, *R6", "fill return address placeholder");
            emit("jump R5", "call function");

            // after the call the return value is inside R1
            emit("push R1", "push return value onto stack");
        }

        void visit(Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
        }

        void visit(IfStatement& statement) override {
            statement.condition->accept(*this);
            emit("pop R1", "get result of condition");
            emit("copy 0, R2", "get constant zero");
            emit("comp R1, R2, R3", "evaluate if condition is true");
            const auto else_label = next_label("else");
            const auto endif_label = next_label("endif");
            emit(fmt::format("jump_eq R3, {}", else_label), "jump to else-block");
            emit("", "begin of then-block");
            statement.then_block.accept(*this);
            emit(fmt::format("jump {}", endif_label), "jump to end of else-block");
            emit_label(else_label, "begin of else-block");
            statement.else_block.accept(*this);
            emit_label(endif_label, "end of else-block");
        }

        void visit(LoopStatement& statement) override {
            const auto loop_start_label = next_label("loop_start");
            const auto loop_end_label = next_label("loop_end");
            loop_stack.push(LoopLabels{
                    .continue_to_label{ loop_start_label },
                    .break_to_label{ loop_end_label }
            });
            emit_label(loop_start_label, "brrrrr");
            statement.body.accept(*this);
            loop_stack.pop();
            emit(fmt::format("jump {}", loop_start_label));
            emit_label(loop_end_label, "end of loop");
        }

        void visit(Parser::Statements::BreakStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.break_token, "break statement not allowed here");
            }
            emit(fmt::format("jump {}", loop_stack.top().break_to_label), "break out of loop");
        }

        void visit(Parser::Statements::ContinueStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.continue_token, "continue statement not allowed here");
            }
            emit(fmt::format("jump {}", loop_stack.top().continue_to_label), "continue to top of loop");
        }

        void visit(VariableDefinition& statement) override {
            using std::ranges::max_element;

            const usize offset =
                    std::get<VariableSymbol>((*statement.surrounding_scope).at(statement.name.location.view())).offset;
            assembly += fmt::format(
                    "\t// new variable called \"{}\" with offset {}\n", statement.name.location.view(), offset
            );
            statement.initial_value->accept(*this);
        }

        void visit(InlineAssembly& statement) override {
            const auto assembly_block = statement.token->location.view();
            const auto start_iterator = std::find(assembly_block.cbegin(), assembly_block.cend(), '{');
            assert(start_iterator != assembly_block.cend());
            const auto inner = std::string_view{ start_iterator + 1, assembly_block.cend() - 1 };
            emit("", "-- block of inline bssembly --");
            emit(inner);
            emit("", "-- end of inline bssembly --");
        }

        void visit(ExpressionStatement& statement) override {
            statement.expression->accept(*this);
            emit("pop R1", "discard value of expression statement");
        }

        [[nodiscard]] std::string next_label(const std::string_view tag) {
            return fmt::format("${}${}", label_counter++, tag);
        }

        usize label_counter{ 0 };

        std::stack<LoopLabels> loop_stack{};
    };

    std::string emit_statement(const Parser::Program& program, Statement& statement) {
        auto visitor = EmitterVisitor{ &program };
        statement.accept(visitor);
        return visitor.assembly;
    }

    std::string Emitter::operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) const {
        const auto mangled_name =
                function_definition->namespace_name + function_definition->corresponding_symbol->signature;
        auto result = fmt::format("\n{}:\n", mangled_name);

        const auto emit = [&result](const std::string_view instruction, const std::string_view comment = "") {
            result += fmt::format("\t{}", instruction);
            if (not comment.empty()) {
                result += fmt::format(" // {}", comment);
            }
            result += "\n";
        };
        if (function_definition->name.location.view() == "main") {
            emit("copy sp, R0", "save current stack pointer into R0 (this is the new stack frame base pointer)");
        }
        result += emit_statement(*program, function_definition->body);
        if (function_definition->name.location.view() == "main") {
            emit("halt");
        } else {
            emit("copy R0, sp", "clear current stack frame");
            emit("pop R0", "restore previous stack frame");
            emit("return");
        }
        return result;
    }

    std::string Emitter::operator()(const std::unique_ptr<Parser::ImportStatement>&) const {
        return "";
    }

}// namespace Emitter
