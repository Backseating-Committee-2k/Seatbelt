//
// Created by coder2k on 24.06.2022.
//

#include "emitter.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "types.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
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

    using namespace Lexer::Tokens;

    struct EmitterVisitor : public ExpressionVisitor, public StatementVisitor {

        // operands are already stored in R1 and R2, result has to be stored in R3
        struct BinaryOperatorEmitter {
            void operator()(const Plus&) const {
                visitor->emit("add R1, R2, R3", "add values");
            }

            void operator()(const Minus&) const {
                visitor->emit("sub R1, R2, R3", "subtract values");
            }

            void operator()(const Asterisk&) const {
                visitor->emit("mult R1, R2, R4, R3", "multiply values");
            }

            void operator()(const ForwardSlash&) const {
                assert(false && "not implemented");
            }

            void operator()(const And&) const {
                visitor->emit("and R1, R2, R3", "and values");
            }

            void operator()(const Or&) const {
                visitor->emit("or R1, R2, R3", "or values");
            }

            void operator()(const EqualsEquals&) const {
                visitor->emit("comp_eq R1, R2, R3");
            }

            void operator()(const ExclamationEquals&) const {
                visitor->emit("comp_neq R1, R2, R3");
            }

            void operator()(const GreaterThan&) const {
                visitor->emit("comp_gt R1, R2, R3");
            }

            void operator()(const GreaterOrEquals&) const {
                visitor->emit("comp_ge R1, R2, R3");
            }

            void operator()(const LessThan&) const {
                visitor->emit("comp_lt R1, R2, R3");
            }

            void operator()(const LessOrEquals&) const {
                visitor->emit("comp_le R1, R2, R3");
            }

            void operator()(const auto&) const {
                assert(false && "unreachable");
            }

            EmitterVisitor* visitor;
        };

        EmitterVisitor(const Parser::Program* program, LabelGenerator* label_generator, std::string_view return_label)
            : program{ program },
              label_generator{ label_generator },
              return_label{ return_label } { }

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
            assert(expression.variable_symbol.has_value() and "if this is not a function, it has to be a variable");

            const auto offset = expression.variable_symbol.value()->offset.value();
            const auto& variable_token = expression.name_tokens.back();
            const auto variable_name = Error::token_location(variable_token).view();

            emit(fmt::format("add R0, {}, R1", offset),
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
                const auto end_of_evaluation = label_generator->next_label("end_of_short_circuiting");
                emit(fmt::format("jump_eq R1, {}", end_of_evaluation), "skip rest of evaluation if value is false");
                expression.rhs->accept(*this);
                emit("pop R2", fmt::format(
                                       R"(store rhs for {}-operator in R2)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                std::visit(BinaryOperatorEmitter{ this }, expression.operator_token);
                const auto after_push = label_generator->next_label("after_push");
                emit(fmt::format("jump {}", after_push));
                emit_label(end_of_evaluation);
                emit("copy R1, R3", "store \"false\" as result");
                emit_label(after_push);
            } else if (is<Or>(expression.operator_token)) {
                emit("pop R1", fmt::format(
                                       R"(store lhs for {}-operator in R1)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                const auto end_of_evaluation = label_generator->next_label("end_of_short_circuiting");
                emit(fmt::format("jump_gt R1, {}", end_of_evaluation), "skip rest of evaluation if value is true");
                expression.rhs->accept(*this);
                emit("pop R2", fmt::format(
                                       R"(store rhs for {}-operator in R2)",
                                       Error::token_location(expression.operator_token).view()
                               ));
                std::visit(BinaryOperatorEmitter{ this }, expression.operator_token);
                const auto after_push = label_generator->next_label("after_push");
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
            emit("\n", "evaluate function call");

            emit("add sp, 12, sp",
                 "reserve stack space for jump address, return address, and old stack frame base pointer");

            usize arguments_size = 0;
            for (const auto& argument : expression.arguments) {
                argument->accept(*this);// evaluate argument => result will be pushed
                arguments_size += argument->data_type->size();
            }

            expression.callee->accept(*this);// evaluate callee => jump address is pushed
            emit("pop R1", "get jump address");
            emit(fmt::format("sub sp, {}, R2", arguments_size + 12), "calculate address of jump address placeholder");
            emit("copy R1, *R2", "store jump address in stack");
            emit("add R2, 8, R2", "calculate address of old stack frame base pointer placeholder");
            emit("copy R0, *R2", "save old stack frame base pointer in stack");

            assert(expression.function_to_call and "function must have been set before");
            assert(expression.function_to_call->body.occupied_stack_space.has_value() and
                   "stack size of function body must be known");
            const auto function_stack_frame_size = expression.function_to_call->body.occupied_stack_space.value();

            emit(fmt::format("add sp, {}, sp", function_stack_frame_size - arguments_size),
                 "reserve stack space for function (excluding arguments)");

            emit(fmt::format("sub sp, {}, R0", function_stack_frame_size), "set new stack frame base pointer");

            emit("add ip, 48, R1", "calculate return address");
            emit(fmt::format("sub sp, {}, R2", 8 + function_stack_frame_size),
                 "calculate address of return address placeholder");
            emit("copy R1, *R2", "fill return address placeholder");

            emit("sub R0, 12, R2", "calculate address of jump address");
            emit("copy *R2, R2", "get jump address");
            emit("jump R2", "call function");
            emit("pop", "pop jump address");

            // after the call the return value is inside R1
            emit("push R1", "push return value onto stack");
        }

        void visit(Assignment& expression) override {
            /* we cannot simply evaluate the assignee since that would only give us the
             * value of the left hand side, so this is a special case */
            const auto assignee = dynamic_cast<const Name*>(expression.assignee.get());
            assert(assignee and "assignee must be a name");
            assert(assignee->variable_symbol.has_value() and "there must be a pointer to the corresponding symbol");
            const auto offset = assignee->variable_symbol.value()->offset.value();

            expression.value->accept(*this);// puts value to assign onto stack
            emit("pop R2", "get value of right side of assignment");
            emit(fmt::format("add R0, {}, R1 ", offset), "get target address of assignment");
            emit("copy R2, *R1", "store value at target address");
            emit("push R2", "push value of assignment expression");
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
            const auto else_label = label_generator->next_label("else");
            const auto endif_label = label_generator->next_label("endif");
            emit(fmt::format("jump_eq R3, {}", else_label), "jump to else-block");
            emit("", "begin of then-block");
            statement.then_block.accept(*this);
            emit(fmt::format("jump {}", endif_label), "jump to end of else-block");
            emit_label(else_label, "begin of else-block");
            statement.else_block.accept(*this);
            emit_label(endif_label, "end of else-block");
        }

        void visit(LoopStatement& statement) override {
            const auto loop_start_label = label_generator->next_label("loop_start");
            const auto loop_end_label = label_generator->next_label("loop_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ loop_start_label }, .break_to_label{ loop_end_label } });
            emit_label(loop_start_label, "brrrrr");
            statement.body.accept(*this);
            loop_stack.pop();
            emit(fmt::format("jump {}", loop_start_label));
            emit_label(loop_end_label, "end of loop");
        }

        void visit(WhileStatement& statement) override {
            const auto while_start_label = label_generator->next_label("while_start");
            const auto while_condition_label = label_generator->next_label("while_condition");
            const auto while_end_label = label_generator->next_label("while_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ while_condition_label },
                                        .break_to_label{ while_end_label } });
            emit(fmt::format("jump {}", while_condition_label), "jump to condition of while-loop");
            emit_label(while_start_label);

            statement.body.accept(*this);

            emit_label(while_condition_label);
            statement.condition->accept(*this);
            emit("pop R1", "get value of while-loop condition");

            emit(fmt::format("jump_gt R1, {}", while_start_label), "repeat the while-loop if the condition is true");

            emit_label(while_end_label);
            loop_stack.pop();
        }

        void visit(DoWhileStatement& statement) override {
            const auto do_while_start_label = label_generator->next_label("do_while_start");
            const auto do_while_condition_label = label_generator->next_label("do_while_condition");
            const auto do_while_end_label = label_generator->next_label("do_while_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ do_while_condition_label },
                                        .break_to_label{ do_while_end_label } });

            emit_label(do_while_start_label);
            statement.body.accept(*this);

            emit_label(do_while_condition_label);
            statement.condition->accept(*this);
            emit("pop R1", "get value of do-while-loop condition");

            emit(fmt::format("jump_gt R1, {}", do_while_start_label),
                 "repeat the do-while-loop if the condition is true");

            emit_label(do_while_end_label);
            loop_stack.pop();
        }

        void visit(ForStatement& statement) override {
            const auto for_start_label = label_generator->next_label("for_start");
            const auto for_condition_label = label_generator->next_label("for_condition");
            const auto for_end_label = label_generator->next_label("for_end");

            loop_stack.push(LoopLabels{ .continue_to_label{ for_condition_label }, .break_to_label{ for_end_label } });

            if (statement.initializer) {
                statement.initializer->accept(*this);
            }

            if (statement.condition) {
                emit(fmt::format("jump {}", for_condition_label), "jump to the condition of the for-loop");
            }

            emit_label(for_start_label);

            statement.body.accept(*this);

            if (statement.increment) {
                statement.increment->accept(*this);
                emit("pop R1", "pop result of increment");
            }

            emit_label(for_condition_label);
            if (statement.condition) {
                statement.condition->accept(*this);
                emit("pop R1", "get value of for-loop condition");
                emit(fmt::format("jump_gt R1, {}", for_start_label), "jump to the beginning of the for-loop");
            } else {
                emit(fmt::format("jump {}", for_start_label), "jump to the beginning of the for-loop");
            }
            emit_label(for_end_label);
            loop_stack.pop();
        }

        void visit(BreakStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.break_token, "break statement not allowed here");
            }
            emit(fmt::format("jump {}", loop_stack.top().break_to_label), "break out of loop");
        }

        void visit(ContinueStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.continue_token, "continue statement not allowed here");
            }
            emit(fmt::format("jump {}", loop_stack.top().continue_to_label), "continue to top of loop");
        }

        void visit(ReturnStatement& statement) override {
            if (statement.return_value) {
                statement.return_value->accept(*this);// evaluate return value => result is pushed
                emit("pop R1", "put return value into R1");
            }
            emit(fmt::format("jump {}", return_label), "immediately exit the current function");
        }

        void visit(VariableDefinition& statement) override {
            const usize offset =
                    std::get<VariableSymbol>((*statement.surrounding_scope).at(statement.name.location.view()))
                            .offset.value();
            assembly += fmt::format(
                    "\t// new variable called \"{}\" with offset {}\n", statement.name.location.view(), offset
            );
            statement.initial_value->accept(*this);// initial value is evaluated and pushed
            emit(fmt::format("add R0, {}, R1", offset),
                 fmt::format("calculate address for variable {}\n", statement.name.location.view()));
            emit("pop R2", "get initial value of variable");
            emit("copy R2, *R1", "store initial value in stack");
        }

        void visit(Parser::Statements::InlineAssembly& statement) override {
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

        void visit(LabelDefinition& statement) override {
            assert(not statement.emitted_label.empty() and "emitted label must have been generated before");
            emit_label(statement.emitted_label);
        }

        void visit(GotoStatement& statement) override {
            assert(statement.target_label != nullptr and "target label must have been set before");
            const auto& label = statement.target_label->emitted_label;
            emit(fmt::format("jump {}", label), fmt::format("goto {}", statement.label_identifier.location.view()));
        }

        LabelGenerator* label_generator;
        std::stack<LoopLabels> loop_stack{};
        std::string_view return_label;
    };

    std::string emit_statement(
            const Parser::Program& program,
            Statement& statement,
            LabelGenerator* label_generator,
            const std::string_view return_label
    ) {
        auto visitor = EmitterVisitor{ &program, label_generator, return_label };
        statement.accept(visitor);
        return visitor.assembly;
    }

    std::string Emitter::operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
        // generate labels for all label definitions in the current function
        for (auto& label : function_definition->contained_labels) {
            label->emitted_label = label_generator->next_label(label->identifier.location.view());
        }


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
        const auto emit_label = [&result](const std::string_view label) { result += fmt::format("{}:\n", label); };

        if (function_definition->is_entry_point) {
            emit("copy sp, R0", "save stack frame base pointer for main function into R0");
            assert(function_definition->body.occupied_stack_space.has_value() and
                   "needed stack size for function must be known");
            emit(fmt::format("add sp, {}, sp", function_definition->body.occupied_stack_space.value()),
                 "reserve stack space for main function");
        }
        const auto function_return_label = label_generator->next_label("function_return");

        result += emit_statement(*program, function_definition->body, label_generator, function_return_label);

        emit_label(function_return_label);

        if (function_definition->is_entry_point) {
            emit("halt");
        } else {
            emit("copy R0, sp", "clear current stack frame");
            emit("pop R0", "restore previous stack frame");
            emit("return");
        }

        return result;
    }

    std::string Emitter::operator()(const std::unique_ptr<Parser::ImportStatement>&) {
        return "";
    }

}// namespace Emitter
