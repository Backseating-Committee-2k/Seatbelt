//
// Created by coder2k on 24.06.2022.
//

#include "emitter.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "types.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <optional>
#include <string_view>

namespace Emitter {
    using namespace Parser::Statements;
    using namespace Parser::Expressions;

    struct EmitterVisitor : public ExpressionVisitor, public StatementVisitor {
        struct BinaryOperatorEmitter {
            void operator()(const Lexer::Tokens::Plus&) const {
                visitor->emit("push R3", "push result onto stack");
            }

            void operator()(const Lexer::Tokens::Minus&) const {
                assert(false && "not implemented");
            }

            void operator()(const Lexer::Tokens::Asterisk&) const {
                visitor->emit("mult R1, R2, R3, R4", "multiply values");
            }

            void operator()(const Lexer::Tokens::ForwardSlash&) const {
                assert(false && "not implemented");
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
                assembly += fmt::format(" // {}", comment);
            }
            assembly += "\n";
        }

        void visit(Literal& expression) override {
            emit(fmt::format("copy {}, R1", expression.value.location.view()), "put immediate into register");
            emit("push R1", "push immediate onto stack");
        }

        void visit(Name& expression) override {
            assert(expression.data_type && "data type must be known at this point");
            if (const auto function_pointer_type = dynamic_cast<const FunctionPointerType*>(expression.data_type)) {
                emit(fmt::format("copy {}, R1", function_pointer_type->signature), "get address of label");
                emit("push R1", "push address of label onto stack");
                return;
            }

            std::optional<usize> offset;
            const Scope* current_scope = expression.surrounding_scope;
            while (current_scope != nullptr) {
                const auto find_iterator = current_scope->find(expression.name.location.view());
                if (find_iterator != current_scope->end()) {
                    offset = std::get<VariableSymbol>(find_iterator->second).offset;
                    break;
                }
                current_scope = current_scope->surrounding_scope;
            }
            if (not offset.has_value()) {
                Error::error(
                        expression.name,
                        fmt::format("use of undeclared identifier \"{}\"", expression.name.location.view())
                );
            }
            emit(fmt::format("add R0, {}, R1", offset.value()),
                 fmt::format("calculate address of variable \"{}\"", expression.name.location.view()));
            emit("copy *R1, R2", fmt::format("load value of variable \"{}\" into R2", expression.name.location.view()));
            emit("push R2",
                 fmt::format("push value of variable \"{}\" onto the stack", expression.name.location.view()));
        }

        void visit(BinaryOperator& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);

            emit("pop R2",
                 fmt::format(
                         R"(store rhs for {}-operator in R2)", Error::token_location(expression.operator_token).view()
                 ));
            emit("pop R1",
                 fmt::format(
                         R"(store lhs for {}-operator in R1)", Error::token_location(expression.operator_token).view()
                 ));

            std::visit(BinaryOperatorEmitter{ this }, expression.operator_token);

            emit("push R3", "push result onto stack");
        }

        void visit(FunctionCall& expression) override {
            expression.callee->accept(*this);// evaluate callee
            emit("pop R5", "get jump address");

            emit("copy sp, R6", "store address of return address placeholder");
            emit("add sp, 4, sp", "reserve stack space for the return address placeholder");

            emit("push R0", "store current stack frame base pointer for later");
            emit("copy sp, R0");

            for (const auto& argument : expression.arguments) {
                argument->accept(*this);
            }

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
            emit(inner);
        }

        void visit(ExpressionStatement& statement) override {
            statement.expression->accept(*this);
            emit("pop R1", "discard value of expression statement");
        }
    };

    std::string emit_statement(const Parser::Program& program, Statement& statement) {
        auto visitor = EmitterVisitor{ &program };
        statement.accept(visitor);
        return visitor.assembly;
    }

    std::string Emitter::operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) const {
        auto result = fmt::format("{}:\n", function_definition->corresponding_symbol->signature);

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
        emit(emit_statement(*program, function_definition->body));
        if (function_definition->name.location.view() == "main") {
            emit("halt");
        } else {
            emit("copy R0, sp", "clear current stack frame");
            emit("pop R0", "restore previous stack frame");
            emit("return");
        }
        return result;
    }

}// namespace Emitter
