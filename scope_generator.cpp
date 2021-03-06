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
        ScopeGenerator(Scope* scope, TypeContainer* type_container)
            : scope{ scope },
              type_container{ type_container } { }

        void visit(Parser::Statements::Block& statement) override {
            statement.surrounding_scope = scope;
            for (auto& sub_statement : statement.statements) {
                if (const auto sub_block = dynamic_cast<Parser::Statements::Block*>(sub_statement.get())) {
                    sub_block->scope =
                            std::make_unique<Scope>(scope, statement.surrounding_scope->surrounding_namespace);
                    auto sub_visitor = ScopeGenerator{ sub_block->scope.get(), type_container };
                    sub_block->accept(sub_visitor);
                } else {
                    sub_statement->accept(*this);
                }
            }
        }

        void visit(Parser::Statements::IfStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.condition->accept(*this);

            statement.then_block.scope =
                    std::make_unique<Scope>(scope, statement.surrounding_scope->surrounding_namespace);
            auto then_visitor = ScopeGenerator{ statement.then_block.scope.get(), type_container };
            statement.then_block.accept(then_visitor);

            statement.else_block.scope =
                    std::make_unique<Scope>(scope, statement.surrounding_scope->surrounding_namespace);
            auto else_visitor = ScopeGenerator{ statement.else_block.scope.get(), type_container };
            statement.else_block.accept(else_visitor);
        }

        void visit(Parser::Statements::LoopStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.body.scope = scope->create_child_scope();
            auto body_visitor = ScopeGenerator{ statement.body.scope.get(), type_container };
            statement.body.accept(body_visitor);
        }

        void visit(Parser::Statements::WhileStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.condition->accept(*this);
            statement.body.scope = scope->create_child_scope();
            auto body_visitor = ScopeGenerator{ statement.body.scope.get(), type_container };
            statement.body.accept(body_visitor);
        }

        void visit(Parser::Statements::DoWhileStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.body.scope = scope->create_child_scope();
            auto body_visitor = ScopeGenerator{ statement.body.scope.get(), type_container };
            statement.body.accept(body_visitor);
            statement.condition->accept(*this);
        }

        void visit(Parser::Statements::ForStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.body.scope = scope->create_child_scope();
            auto body_visitor = ScopeGenerator{ statement.body.scope.get(), type_container };
            if (statement.initializer) {
                statement.initializer->accept(body_visitor);
            }
            if (statement.condition) {
                statement.condition->accept(body_visitor);
            }
            if (statement.increment) {
                statement.increment->accept(body_visitor);
            }
            statement.body.accept(body_visitor);
        }

        void visit(Parser::Statements::ReturnStatement& statement) override {
            statement.surrounding_scope = scope;
            if (statement.return_value) {
                statement.return_value->accept(*this);
            }
        }

        void visit(Parser::Statements::BreakStatement&) override { }

        void visit(Parser::Statements::ContinueStatement&) override { }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            if (scope->contains(statement.name.location.view())) {
                Error::error(
                        statement.name, fmt::format("redefinition of identifier \"{}\"", statement.name.location.view())
                );
            }
            statement.initial_value->accept(*this);
            (*scope)[statement.name.location.view()] = VariableSymbol{ .definition{ &statement } };
            statement.variable_symbol = &std::get<VariableSymbol>(scope->at(statement.name.location.view()));
            assert(statement.variable_symbol != nullptr);
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::InlineAssembly& statement) override {
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::ExpressionStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.expression->accept(*this);
        }

        void visit(Parser::Expressions::Integer& expression) override {
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::Char& expression) override {
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::Bool& expression) override {
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
                    std::visit(
                            overloaded{
                                    [&](const VariableSymbol& variable) { expression.variable_symbol = &variable; },
                                    [remaining = namespace_qualifier, &identifier_token,
                                     &expression](const FunctionSymbol& function) mutable {
                                        using std::ranges::count;
                                        // We *did* find a function symbol with the correct function name (even though we
                                        // do not know the function signature), but the function can only be a valid choice
                                        // if it is within the correct namespace
                                        auto remaining_namespaces = remaining.empty() ? 0 : count(remaining, '%') + 1;
                                        const auto was_qualified_name = (expression.name_tokens.size() > 1);
                                        auto possible_overloads = std::vector<const FunctionOverload*>{};
                                        while (true) {
                                            // Function definitions are only allowed in the global scope. That means
                                            // that we must be at the top of the scope stack right now.
                                            for (const auto& overload : function.overloads) {
                                                if (overload.namespace_name == remaining) {
                                                    // we cannot determine the return type of the function here because
                                                    // we cannot do overload resolution as of now
                                                    possible_overloads.push_back(&overload);
                                                }
                                            }

                                            // if the name is a qualified name (i.e. with specified namespace) it is
                                            // not valid to make a lookup in the outer namespaces
                                            if (was_qualified_name) {
                                                break;
                                            }

                                            if (remaining_namespaces == 0) {
                                                break;
                                            }
                                            if (remaining_namespaces == 1) {
                                                remaining = "";
                                            } else {
                                                const auto end_index = remaining.rfind('%');
                                                assert(end_index != decltype(remaining)::npos);
                                                remaining = remaining.substr(0, end_index);
                                            }
                                            --remaining_namespaces;
                                        }

                                        // if we couldn't find any overloads yet and this is a qualified name,
                                        // then we can also look into the global namespace and try to find an
                                        // exact match in there
                                        const auto is_global_lookup_fallback = possible_overloads.empty();

                                        const auto inside_global_namespace =
                                                expression.surrounding_scope->surrounding_namespace.empty();

                                        if (was_qualified_name) {
                                            const auto global_name_qualifier =
                                                    get_absolute_namespace_qualifier(expression);
                                            for (const auto& overload : function.overloads) {
                                                if (overload.namespace_name == global_name_qualifier) {
                                                    // we cannot determine the return type of the function here because
                                                    // we cannot do overload resolution as of now
                                                    if (is_global_lookup_fallback) {
                                                        possible_overloads.push_back(&overload);
                                                    } else if (not inside_global_namespace) {
                                                        Error::warning(
                                                                expression.name_tokens.back(),
                                                                "absolute/relative namespace ambiguity can lead to "
                                                                "confusion"
                                                        );
                                                    }
                                                }
                                            }
                                        }

                                        if (possible_overloads.empty()) {
                                            Error::error(identifier_token, "no matching function overload found");
                                        }
                                        expression.possible_overloads = std::move(possible_overloads);
                                    } },
                            find_iterator->second
                    );
                    return;
                }
                current_scope = current_scope->surrounding_scope;
            }
            Error::error(identifier_token, "use of undeclared identifier");
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

        void visit(Parser::Expressions::Assignment& expression) override {
            expression.surrounding_scope = scope;
            expression.assignee->accept(*this);
            expression.value->accept(*this);
        }

        Scope* scope;
        TypeContainer* type_container;
    };

    struct ScopeGeneratorBodyVisitor {
        ScopeGeneratorBodyVisitor(Parser::Program* program, TypeContainer* type_container, Scope* global_scope)
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            auto function_scope = std::make_unique<Scope>(global_scope, function_definition->namespace_name);
            for (auto& parameter : function_definition->parameters) {
                if (function_scope->contains(parameter.name.location.view())) {
                    Error::error(
                            parameter.name,
                            fmt::format("duplicate parameter name \"{}\"", parameter.name.location.view())
                    );
                }
                (*function_scope)[parameter.name.location.view()] = VariableSymbol{ .definition{ &parameter } };
                parameter.variable_symbol =
                        &std::get<VariableSymbol>(function_scope->at(parameter.name.location.view()));
            }

            auto visitor = ScopeGenerator{ function_scope.get(), type_container };
            function_definition->body.scope = std::move(function_scope);
            function_definition->body.accept(visitor);
        }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        Parser::Program* program;
        TypeContainer* type_container;
        Scope* global_scope;
    };

    struct ScopeGeneratorTopLevelVisitor {
        ScopeGeneratorTopLevelVisitor(Parser::Program* program, TypeContainer* type_container, Scope* global_scope)
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using namespace std::string_literals;
            using std::ranges::find_if;

            auto identifier = function_definition->name.location.view();
            auto find_iterator = find_if(*global_scope, [&](const auto& pair) { return pair.first == identifier; });
            const auto found = find_iterator != std::end(*global_scope);
            auto function_overload = FunctionOverload{ .namespace_name{ function_definition->namespace_name },
                                                       .definition{ function_definition.get() } };

            if (found) {
                assert(std::holds_alternative<FunctionSymbol>(find_iterator->second) &&
                       "other cases not implemented yet");
                auto& function_symbol = std::get<FunctionSymbol>(find_iterator->second);
                auto& new_overload = function_symbol.overloads.emplace_back(std::move(function_overload));
                function_definition->corresponding_symbol = &new_overload;
                assert(std::get<FunctionSymbol>((*global_scope)[identifier]).overloads.size() > 1);
            } else {
                auto new_symbol = FunctionSymbol{ .overloads{ { std::move(function_overload) } } };
                function_definition->corresponding_symbol = &new_symbol.overloads.back();
                (*global_scope)[identifier] = std::move(new_symbol);
            }
        }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        Parser::Program* program;
        TypeContainer* type_container;
        Scope* global_scope;
    };

    void visit_function_definitions(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        auto visitor = ScopeGeneratorTopLevelVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    void visit_function_bodies(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        auto visitor = ScopeGeneratorBodyVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    void generate(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        visit_function_definitions(program, type_container, global_scope);
        visit_function_bodies(program, type_container, global_scope);
    }

}// namespace ScopeGenerator
