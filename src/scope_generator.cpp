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

    [[nodiscard]] std::vector<const FunctionOverload*> namespace_based_lookup(
            std::string_view namespace_qualifier,
            const Parser::Expressions::Name& name,
            const FunctionSymbol& function
    ) {
        using std::ranges::count;
        // We *did* find a function symbol with the correct function name (even though we
        // do not know the function signature), but the function can only be a valid choice
        // if it is within the correct namespace

        auto remaining_namespaces = namespace_qualifier.empty() ? 0 : count(namespace_qualifier, ':') / 2;
        const auto was_qualified_name = (name.name_tokens.size() > 1);
        auto possible_overloads = std::vector<const FunctionOverload*>{};
        while (true) {
            // Function definitions are only allowed in the global scope. That means
            // that we must be at the top of the scope stack right now.
            for (const auto& overload : function.overloads) {
                if (overload.namespace_name == namespace_qualifier) {
                    // we found a possible overload!

                    // we cannot determine the return type of the function here because
                    // we cannot do type-based overload resolution as of now

                    // if the namespace the name expression occurred in is different from the namespace
                    // in which we found the function overload, then the lookup is only valid if the function
                    // has been exported
                    if (name.surrounding_scope->surrounding_namespace.starts_with(namespace_qualifier) or
                        overload.definition->is_exported()) {
                        possible_overloads.push_back(&overload);
                    }
                }
            }

            // if the name is a qualified name (i.e. with specified namespace) it is
            // not valid to make a lookup in the outer namespaces (except for the
            // global namespace, but that case is handled separately later on)
            if (was_qualified_name) {
                break;
            }

            if (remaining_namespaces == 1) {
                break;
            }
            const auto max_pos = namespace_qualifier.length() >= 3 ? namespace_qualifier.length() - 3
                                                                   : decltype(namespace_qualifier)::npos;
            const auto end_index = namespace_qualifier.rfind(':', max_pos);
            assert(end_index != decltype(namespace_qualifier)::npos);
            namespace_qualifier = namespace_qualifier.substr(0, end_index + 1);
            --remaining_namespaces;
        }

        // if we couldn't find any overloads yet and this is a qualified name,
        // then we can also look into the global namespace and try to find an
        // exact match in there
        const auto is_global_lookup_fallback = possible_overloads.empty();

        const auto inside_global_namespace = name.surrounding_scope->surrounding_namespace == "::";

        if (was_qualified_name) {
            const auto global_name_qualifier = get_absolute_namespace_qualifier(name);
            for (const auto& overload : function.overloads) {
                if (overload.namespace_name == global_name_qualifier) {
                    // we found a possible overload!

                    // we cannot determine the return type of the function here because
                    // we cannot do type-based overload resolution as of now
                    if (is_global_lookup_fallback) {
                        // during fall-back, only exported functions are valid
                        if (overload.definition->is_exported()) {
                            possible_overloads.push_back(&overload);
                        }
                    } else if (not inside_global_namespace) {
                        Error::warning(
                                name.name_tokens.back(),
                                "absolute/relative namespace ambiguity can lead to "
                                "confusion"
                        );
                    }
                }
            }
        }

        if (possible_overloads.empty()) {
            Error::error(name.name_tokens.back(), "no matching function overload found");
        }
        return possible_overloads;
    }

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

        void visit(Parser::Statements::LabelDefinition& statement) override {
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::GotoStatement& statement) override {
            statement.surrounding_scope = scope;
            const auto function = statement.surrounding_function;
            assert(function != nullptr and "surrounding function must have been set before");
            const auto find_iterator = std::find_if(
                    std::cbegin(function->contained_labels), std::cend(function->contained_labels),
                    [&](const auto label_definition) {
                        return label_definition->identifier.location.view() ==
                               statement.label_identifier.location.view();
                    }
            );
            const auto found = find_iterator != std::cend(function->contained_labels);
            if (not found) {
                Error::error(
                        statement.goto_token, fmt::format(
                                                      "no label named \"{}\" found within the surrounding function",
                                                      statement.label_identifier.location.view()
                                              )
                );
            }
            statement.target_label = *find_iterator;
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
                            overloaded{ [&](const VariableSymbol& variable) { expression.variable_symbol = &variable; },
                                        [&](const FunctionSymbol& function) {
                                            expression.possible_overloads =
                                                    namespace_based_lookup(namespace_qualifier, expression, function);
                                        } },
                            find_iterator->second
                    );
                    return;
                }
                current_scope = current_scope->surrounding_scope;
            }
            Error::error(identifier_token, "use of undeclared identifier");
        }

        void visit(Parser::Expressions::UnaryOperator& expression) override {
            expression.operand->accept(*this);
            expression.surrounding_scope = scope;
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

        void visit(Parser::Expressions::Nothing& expression) override {
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::TypeSizeExpression& expression) override {
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::ValueSizeExpression& expression) override {
            expression.expression->accept(*this);
            expression.surrounding_scope = scope;
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

    /**
     * This visitor only takes care of labels. This is needed for `goto`-statements. For this
     * reason, this visitor does nothing with the most statements and expressions.
     */
    struct ScopeGeneratorLabelVisitor : public Parser::Statements::StatementVisitor,
                                        public Parser::Expressions::ExpressionVisitor {
        explicit ScopeGeneratorLabelVisitor(Parser::FunctionDefinition* surrounding_function)
            : surrounding_function{ surrounding_function } { }

        void visit(Parser::Statements::Block& statement) override {
            for (const auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
        }

        void visit(Parser::Statements::LabelDefinition& statement) override {
            using std::ranges::find_if;
            const auto find_iterator = find_if(surrounding_function->contained_labels, [&](const auto label) {
                return label->identifier.location.view() == statement.identifier.location.view();
            });
            const auto found = find_iterator != std::cend(surrounding_function->contained_labels);
            if (found) {
                Error::error(
                        statement.label_token,
                        fmt::format("duplicate label identifier \"{}\"", statement.identifier.location.view())
                );
            }
            surrounding_function->contained_labels.push_back(&statement);
        }

        void visit(Parser::Statements::GotoStatement& statement) override {
            statement.surrounding_function = surrounding_function;
        }

        void visit(Parser::Statements::IfStatement& statement) override {
            statement.then_block.accept(*this);
            statement.else_block.accept(*this);
        }

        void visit(Parser::Statements::LoopStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::WhileStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::DoWhileStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::ForStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::BreakStatement&) override { }
        void visit(Parser::Statements::ContinueStatement&) override { }
        void visit(Parser::Statements::ReturnStatement&) override { }
        void visit(Parser::Statements::VariableDefinition&) override { }
        void visit(Parser::Statements::InlineAssembly&) override { }
        void visit(Parser::Statements::ExpressionStatement&) override { }
        void visit(Parser::Expressions::Integer&) override { }
        void visit(Parser::Expressions::Char&) override { }
        void visit(Parser::Expressions::Bool&) override { }
        void visit(Parser::Expressions::Name&) override { }
        void visit(Parser::Expressions::UnaryOperator&) override { }
        void visit(Parser::Expressions::BinaryOperator&) override { }
        void visit(Parser::Expressions::FunctionCall&) override { }
        void visit(Parser::Expressions::Assignment&) override { }
        void visit(Parser::Expressions::Nothing&) override { }
        void visit(Parser::Expressions::TypeSizeExpression&) override { }
        void visit(Parser::Expressions::ValueSizeExpression&) override { }

        Parser::FunctionDefinition* surrounding_function;
    };

    struct ScopeGeneratorTopLevelVisitor {
        ScopeGeneratorTopLevelVisitor(Parser::Program* program, TypeContainer* type_container, Scope* global_scope)
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using namespace std::string_literals;
            using std::ranges::find_if;

            if (function_definition->export_token.has_value() and function_definition->namespace_name == "::") {
                Error::error(
                        *(function_definition->export_token),
                        "exporting functions from the global namespace is not allowed"
                );
            }

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

            auto label_visitor = ScopeGeneratorLabelVisitor{ function_definition.get() };
            function_definition->body.accept(label_visitor);
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
