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

namespace Emitter {
    using namespace Parser::Statements;
    using namespace Parser::Expressions;

    using SymbolTable = std::unordered_map<std::string_view, usize>;
    using ScopeStack = std::vector<SymbolTable>;

    struct ExpressionEmitterVisitor : public ExpressionVisitor {
        explicit ExpressionEmitterVisitor(const ScopeStack* scopes) : scopes{ scopes } { }

        std::string assembly;
        const ScopeStack* scopes;

        void visit(Literal& expression) override {
            assembly += std::format("\tcopy {}, R1 // put immediate into register\n", expression.value.location.view());
            assembly += std::format("\tpush R1 // push immediate onto stack\n");
        }

        void visit(Name& expression) override {
            std::optional<usize> offset;
            for (auto iterator = scopes->crbegin(); iterator != scopes->crend(); ++iterator) {
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
            assembly += std::format(
                    "\tadd R0, {}, R1 // calculate address of variable \"{}\"\n", offset.value(),
                    expression.name.location.view()
            );
            assembly += std::format(
                    "\tcopy *R1, R2 // load value of variable \"{}\" into R2\n", expression.name.location.view()
            );
            assembly += std::format(
                    "\tpush R2 // push value of variable \"{}\" onto the stack\n", expression.name.location.view()
            );
        }

        void visit(Addition& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            assembly += "\tpop R2 // store rhs of addition in R2\n";
            assembly += "\tpop R1 // store lhs of addition in R1\n";
            assembly += "\tadd R1, R2, R3 // add values\n";
            assembly += "\tpush R3 // push result onto stack\n";
        }

        void visit(Subtraction& expression) override { }

        void visit(Multiplication& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            assembly += "\tpop R2 // store rhs of addition in R2\n";
            assembly += "\tpop R1 // store lhs of addition in R1\n";
            assembly += "\tmult R1, R2, R3, R4 // multiply values\n";
            assembly += "\tpush R4 // push result onto stack\n";
        }

        void visit(Division& expression) override { }
    };

    struct StatementEmitterVisitor : public Parser::Statements::StatementVisitor {

        explicit StatementEmitterVisitor(SymbolTable&& scope) : scopes{ { std::move(scope) } } {
            current_offset = 4 * scopes.back().size();// TODO: different data types
        }

        std::string assembly;
        ScopeStack scopes;
        usize current_offset;

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
            auto expression_visitor = ExpressionEmitterVisitor{ &scopes };
            statement.initial_value->accept(expression_visitor);
            assembly += expression_visitor.assembly;
            scopes.back()[statement.name.location.view()] = current_offset;
            current_offset += 4;// TODO: different data types
        }
    };

    std::string emit_statement(Statement& statement, SymbolTable&& surrounding_scope) {
        auto visitor = StatementEmitterVisitor{ std::move(surrounding_scope) };
        statement.accept(visitor);
        return visitor.assembly;
    }

    std::string Emitter::operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) const {
        auto result = std::string{ function_definition->name.location.view() } + ":\n";
        // TODO: emit instruction to store stack frame base pointer
        result += "\tpush R0 // save old stack frame base pointer for later\n";
        result += "\tcopy R255, R0 // save current stack pointer into R0 (this is the new stack frame base pointer)\n";
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
        result += std::format("{}\n", emit_statement(function_definition->body, std::move(scope)));
        result += "\tcopy R0, R255 // delete current stack frame\n";
        result += "\tpop R0 // set parent stack frame base pointer as new stack frame base pointer\n";
        return result;
    }

}// namespace Emitter
