//
// Created by coder2k on 12.07.2022.
//

#include "scope_generator.hpp"
#include "error.hpp"
#include "namespace.hpp"
#include "parser.hpp"
#include "type_checker.hpp"
#include "types.hpp"
#include <algorithm>
#include <fmt/core.h>
#include <fmt/format.h>
#include <ranges>

namespace ScopeGenerator {

    struct ScopeGenerator : public Parser::Statements::StatementVisitor, public Parser::Expressions::ExpressionVisitor {
        ScopeGenerator(Scope* scope, usize offset, TypeContainer* type_container)
            : scope{ scope },
              offset{ offset },
              type_container{ type_container } { }

        void visit(Parser::Statements::Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                if (const auto sub_block = dynamic_cast<Parser::Statements::Block*>(sub_statement.get())) {
                    sub_block->scope =
                            std::make_unique<Scope>(scope, statement.surrounding_scope->surrounding_namespace);
                    auto sub_visitor = ScopeGenerator{ sub_block->scope.get(), offset, type_container };
                    sub_block->accept(sub_visitor);
                    offset = sub_visitor.offset;
                } else {
                    sub_statement->accept(*this);
                }
            }
        }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            if (scope->contains(statement.name.location.view())) {
                Error::error(
                        statement.name, fmt::format("redefinition of identifier \"{}\"", statement.name.location.view())
                );
            }
            statement.initial_value->accept(*this);
            const auto data_type = type_container->from_tokens(statement.type_tokens);
            (*scope)[statement.name.location.view()] = VariableSymbol{ .offset{ offset }, .data_type{ data_type } };
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
            using std::ranges::find_if, Lexer::Tokens::Identifier;
            expression.surrounding_scope = scope;
            const Scope* current_scope = expression.surrounding_scope;
            const auto namespace_qualifier = get_namespace_qualifier(expression);

            // only the last token of the qualified name is relevant for lookup
            const auto& identifier_token = expression.name_tokens.back();
            const auto identifier = Error::token_location(identifier_token).view();

            while (current_scope != nullptr) {
                const auto find_iterator =
                        find_if(*current_scope, [identifier](const auto& pair) { return pair.first == identifier; });
                const auto identifier_found = find_iterator != std::cend(*current_scope);
                if (identifier_found) {
                    struct {
                        const DataType* operator()(const VariableSymbol& variable) {
                            return variable.data_type;
                        }

                        const DataType* operator()(const FunctionSymbol& function) {
                            // Function definitions are only allowed in the global scope. That means
                            // that we must be at the top of the scope stack right now.
                            const auto overload_result = find_if(function.overloads, [&](const auto& overload) {
                                return overload.namespace_name == namespace_qualifier;
                            });
                            const auto overload_found = (overload_result != std::cend(function.overloads));
                            if (not overload_found) {
                                // We *did* find a function symbol with the correct function name (even though we
                                // do not know the function signature), but the function can only be a valid choice
                                // if it is in the correct namespace.
                                Error::error(
                                        identifier_token,
                                        fmt::format("no function named \"{}\" in the current namespace", identifier)
                                );
                            }

                            // we cannot determine the return type of the function here because
                            // we cannot do overload resolution as of now
                            return nullptr;
                        }

                        const std::string& namespace_qualifier;
                        const Lexer::Tokens::Token& identifier_token;
                        const std::string_view& identifier;
                    } symbol_visitor{ namespace_qualifier, identifier_token, identifier };

                    expression.definition_data_type = std::visit(symbol_visitor, find_iterator->second);
                    return;
                }
                current_scope = current_scope->surrounding_scope;
            }
            // at this point it is still possible that the identifier refers to a function
            // which we haven't seen yet, so this must not be an error
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

        Scope* scope;
        usize offset;
        TypeContainer* type_container;
    };

    struct TopLevelScopeGeneratorVisitor {
        TopLevelScopeGeneratorVisitor(Parser::Program* program, TypeContainer* type_container, Scope* global_scope)
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using namespace std::string_literals;
            using std::ranges::find_if;

            auto function_scope = std::make_unique<Scope>(global_scope, function_definition->namespace_name);
            usize offset = 0;
            for (auto& parameter : function_definition->parameters) {
                if (function_scope->contains(parameter.name.location.view())) {
                    Error::error(
                            parameter.name,
                            fmt::format("duplicate parameter name \"{}\"", parameter.name.location.view())
                    );
                }
                const auto parameter_type = type_container->from_tokens(parameter.type_tokens);
                (*function_scope)[parameter.name.location.view()] =
                        VariableSymbol{ .offset{ offset }, .data_type{ parameter_type } };
                offset += 4;// TODO: different data types
            }
            auto identifier = function_definition->name.location.view();
            auto find_iterator = find_if(*global_scope, [&](const auto& pair) { return pair.first == identifier; });
            const auto found = find_iterator != std::end(*global_scope);
            auto function_overload = FunctionOverload{ .namespace_name{ function_definition->namespace_name } };
            if (found) {
                assert(std::holds_alternative<FunctionSymbol>(find_iterator->second) &&
                       "other cases not implemented yet");
                auto& function_symbol = std::get<FunctionSymbol>(find_iterator->second);
                auto& new_overload = function_symbol.overloads.emplace_back(std::move(function_overload));
                function_definition->corresponding_symbol = &new_overload;
            } else {
                auto new_symbol = FunctionSymbol{ .overloads{ { std::move(function_overload) } } };
                function_definition->corresponding_symbol = &new_symbol.overloads.back();
                (*global_scope)[identifier] = std::move(new_symbol);
            }

            auto visitor = ScopeGenerator{ function_scope.get(), offset, type_container };
            function_definition->body.scope = std::move(function_scope);
            function_definition->body.accept(visitor);
        }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        Parser::Program* program;
        TypeContainer* type_container;
        Scope* global_scope;
    };

    void generate(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        auto visitor = TopLevelScopeGeneratorVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

}// namespace ScopeGenerator
