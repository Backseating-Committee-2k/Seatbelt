//
// Created by coder2k on 24.06.2022.
//

#include "emitter.hpp"
#include "parser.hpp"
#include "types.hpp"
#include "error.hpp"
#include <unordered_map>
#include <string_view>
#include <format>
#include <vector>
#include <algorithm>
#include <optional>
#include <cassert>

namespace Emitter {
    using namespace Parser::Statements;
    using namespace Parser::Expressions;

    using SymbolTable = std::unordered_map<std::string_view, usize>;
    using ScopeStack = std::vector<SymbolTable>;

    struct EmitterVisitor : public ExpressionVisitor, public StatementVisitor {
        EmitterVisitor(const Parser::Program* program, SymbolTable&& scope)
            : program{ program },
              scopes{ { std::move(scope) } } {
            current_offset = 4 * scopes.back().size();// TODO: different data types
        }

        std::string assembly;
        const Parser::Program* program;
        ScopeStack scopes;
        usize current_offset;

        void emit(const std::string_view instruction) {
            emit(instruction, "");
        }

        void emit(const std::string_view instruction, const std::string_view comment) {
            assembly += std::format("\t{}", instruction);
            if (not comment.empty()) {
                assembly += std::format(" // {}", comment);
            }
            assembly += "\n";
        }

        void visit(Literal& expression) override {
            emit(std::format("copy {}, R1", expression.value.location.view()), "put immediate into register");
            emit("push R1", "push immediate onto stack");
        }

        void visit(Name& expression) override {
            for (const auto& top_level_statement : *program) {
                if (std::holds_alternative<std::unique_ptr<Parser::FunctionDefinition>>(top_level_statement)) {
                    const auto function_definition =
                            std::get<std::unique_ptr<Parser::FunctionDefinition>>(top_level_statement).get();
                    if (function_definition->name.location.view() == expression.name.location.view()) {
                        emit(std::format("copy {}, R1", function_definition->name.location.view()),
                             "get address of label");
                        emit("push R1", "push address of label onto stack");
                        return;
                    }
                }
            }

            std::optional<usize> offset;
            for (auto iterator = scopes.crbegin(); iterator != scopes.crend(); ++iterator) {
                const auto find_iterator = iterator->find(expression.name.location.view());
                if (find_iterator != iterator->end()) {
                    offset = find_iterator->second;
                    break;
                }
            }
            if (not offset.has_value()) {
                Error::error(
                        expression.name,
                        std::format("use of undeclared identifier \"{}\"", expression.name.location.view())
                );
            }
            emit(std::format("add R0, {}, R1", offset.value()),
                 std::format("calculate address of variable \"{}\"", expression.name.location.view()));
            emit("copy *R1, R2", std::format("load value of variable \"{}\" into R2", expression.name.location.view()));
            emit("push R2",
                 std::format("push value of variable \"{}\" onto the stack", expression.name.location.view()));
        }

        void visit(Addition& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);

            emit("pop R2", "store rhs of addition in R2");
            emit("pop R1", "store lhs of addition in R1");
            emit("add R1, R2, R3", "add values");
            emit("push R3", "push result onto stack");
        }

        void visit(Subtraction& expression) override { }

        void visit(Multiplication& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            emit("pop R2", "store rhs of addition in R2");
            emit("pop R1", "store lhs of addition in R1");
            emit("mult R1, R2, R3, R4", "multiply values");
            emit("push R4", "push result onto stack");
        }

        void visit(Division& expression) override { }

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
                const bool is_block = static_cast<bool>(dynamic_cast<const Block*>(sub_statement.get()));
                if (is_block) {
                    scopes.emplace_back();
                }
                sub_statement->accept(*this);
                if (is_block) {
                    current_offset -= 4 * scopes.back().size();
                    scopes.pop_back();
                }
            }
        }

        void visit(VariableDefinition& statement) override {
            using std::ranges::max_element;

            if (scopes.back().contains(statement.name.location.view())) {
                Error::error(
                        statement.name, std::format("redefinition of identifier \"{}\"", statement.name.location.view())
                );
            }
            assembly += std::format(
                    "\t// new variable called \"{}\" with offset {}\n", statement.name.location.view(), current_offset
            );
            statement.initial_value->accept(*this);
            scopes.back()[statement.name.location.view()] = current_offset;
            current_offset += 4;// TODO: different data types
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

    std::string emit_statement(const Parser::Program& program, Statement& statement, SymbolTable&& surrounding_scope) {
        auto visitor = EmitterVisitor{ &program, std::move(surrounding_scope) };
        statement.accept(visitor);
        return visitor.assembly;
    }

    std::string Emitter::operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) const {
        auto result = std::string{ function_definition->name.location.view() } + ":\n";
        const auto emit = [&result](const std::string_view instruction, const std::string_view comment = "") {
            result += std::format("\t{}", instruction);
            if (not comment.empty()) {
                result += std::format(" // {}", comment);
            }
            result += "\n";
        };
        if (function_definition->name.location.view() == "main") {
            emit("copy sp, R0", "save current stack pointer into R0 (this is the new stack frame base pointer)");
        }
        auto scope = SymbolTable{};
        usize offset = 0;
        for (const auto& parameter : function_definition->parameters) {
            if (scope.contains(parameter.name.location.view())) {
                Error::error(
                        parameter.name, std::format("duplicate parameter name \"{}\"", parameter.name.location.view())
                );
            }
            scope[parameter.name.location.view()] = offset;
            offset += 4;// TODO: different data types?
        }
        emit(emit_statement(*program, function_definition->body, std::move(scope)));
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
