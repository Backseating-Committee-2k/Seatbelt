//
// Created by coder2k on 12.07.2022.
//

#include "scope_generator.hpp"
#include "parser.hpp"
#include "types.hpp"
#include "error.hpp"
#include "type_checker.hpp"
#include <fmt/core.h>

namespace ScopeGenerator {

    struct ScopeGenerator : public Parser::Statements::StatementVisitor, public Parser::Expressions::ExpressionVisitor {
        ScopeGenerator(SymbolTable* scope, usize offset, TypeContainer* type_container)
            : scope{ scope },
              offset{ offset },
              type_container{ type_container } { }

        void visit(Parser::Statements::Block& statement) override {
            statement.surrounding_scope = scope;
            statement.scope = std::make_unique<SymbolTable>(scope);
            auto visitor = ScopeGenerator{ statement.scope.get(), offset, type_container };
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(visitor);
            }
            offset = visitor.offset;
        }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            if (scope->contains(statement.name.location.view())) {
                Error::error(
                        statement.name, fmt::format("redefinition of identifier \"{}\"", statement.name.location.view())
                );
            }
            statement.initial_value->accept(*this);
            const auto data_type = type_container->from_tokens(statement.type_tokens);
            (*scope)[statement.name.location.view()] =
                    SymbolDescription{ .offset{ offset }, .data_type{ data_type } };
            offset += 4;// TODO: different data types
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::InlineAssembly& statement) override {
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::ExpressionStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.expression->accept(*this);
        }

        void visit(Parser::Expressions::Literal& expression) override {
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::Name& expression) override {
            expression.surrounding_scope = scope;
            const SymbolTable* current_scope = expression.surrounding_scope;
            const auto identifier = expression.name.location.view();
            while (current_scope != nullptr) {
                const auto find_iterator = std::find_if(
                        std::cbegin(*current_scope), std::cend(*current_scope),
                        [identifier](const auto& pair) { return pair.first == identifier; }
                );
                if (find_iterator != std::cend(*current_scope)) {
                    // identifier found => can be used here
                    return;
                }
                current_scope = current_scope->surrounding_scope;
            }
            Error::error(expression.name, fmt::format(R"(use of undeclared identifier "{}")", identifier));
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::FunctionCall& expression) override {
            expression.callee->accept(*this);
            for (auto& argument : expression.arguments) {
                argument->accept(*this);
            }
            expression.surrounding_scope = scope;
        }

        SymbolTable* scope;
        usize offset;
        TypeContainer* type_container;
    };

    void generate(Parser::Program& program, TypeContainer& type_container) {
        auto global_scope = SymbolTable{ nullptr };
        for (auto& top_level_statement : program) {
            std::visit(
                    [&](std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
                        auto function_scope = SymbolTable{ &global_scope };
                        usize offset = 0;
                        for (auto& parameter : function_definition->parameters) {
                            if (function_scope.contains(parameter.name.location.view())) {
                                Error::error(
                                        parameter.name,
                                        fmt::format("duplicate parameter name \"{}\"", parameter.name.location.view())
                                );
                            }
                            const auto parameter_type = type_container.from_tokens(parameter.type_tokens);
                            function_scope[parameter.name.location.view()] =
                                    SymbolDescription{ .offset{ offset }, .data_type{ parameter_type } };
                            offset += 4;// TODO: different data types
                        }
                        auto visitor = ScopeGenerator{ &function_scope, offset, &type_container };
                        function_definition->body.accept(visitor);
                    },
                    top_level_statement
            );
        }
    }

}// namespace ScopeGenerator
