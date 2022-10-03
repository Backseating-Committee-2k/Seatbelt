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
#include <variant>

namespace ScopeGenerator {

    struct StructLookupResult {
        const Parser::VariantDefinition* struct_definition;
    };

    struct CustomTypeLookupResult {
        const Parser::CustomTypeDefinition* custom_type_definition;
    };

    struct NothingFoundLookupResult { };

    using LookupResult = std::variant<StructLookupResult, CustomTypeLookupResult, NothingFoundLookupResult>;

    [[nodiscard]] static std::optional<const Scope*>
    get_relative_scope(const Scope* starting_scope, const Parser::Expressions::Name& name_expression) {
        using std::ranges::find_if;
        for (usize i = 0; i < name_expression.name_tokens.size() - 1; i += 2) {
            const auto namespace_identifier = std::get<Parser::Identifier>(name_expression.name_tokens[i]);
            const auto find_iterator = find_if(*starting_scope, [&](const auto& pair) {
                return pair.first == namespace_identifier.location.view()
                       and std::holds_alternative<NamespaceSymbol>(pair.second);
            });
            const auto found = (find_iterator != starting_scope->cend());
            if (not found) {
                return {};
            }
            starting_scope = std::get<NamespaceSymbol>(find_iterator->second).namespace_definition->scope.get();
        }
        return starting_scope;
    }

    [[nodiscard]] static const Scope* get_global_scope(const Scope* starting_scope) {
        while (starting_scope->surrounding_scope != nullptr) {
            starting_scope = starting_scope->surrounding_scope;
        }
        return starting_scope;
    }

    template<typename T>
    [[nodiscard]] T get_symbols_in_scope(const Scope* scope, Parser::Identifier name, bool exported_only);

    template<>
    [[nodiscard]] std::vector<const FunctionOverload*>
    get_symbols_in_scope(const Scope* scope, const Parser::Identifier name, bool exported_only) {
        using std::ranges::find_if;
        auto result = std::vector<const FunctionOverload*>{};
        const auto find_iterator = find_if(*scope, [&](const auto& pair) {
            return pair.first == name.location.view() and std::holds_alternative<FunctionSymbol>(pair.second);
        });
        const auto found = (find_iterator != scope->cend());
        if (found) {
            for (const auto& overload : std::get<FunctionSymbol>(find_iterator->second).overloads) {
                if (not exported_only or overload.definition->is_exported()) {
                    result.push_back(&overload);
                }
            }
        }
        return result;
    }

    template<>
    [[nodiscard]] const Parser::CustomTypeDefinition*
    get_symbols_in_scope(const Scope* scope, const Parser::Identifier name, bool exported_only) {
        using std::ranges::find_if;
        const auto find_iterator = find_if(*scope, [&](const auto& pair) {
            return pair.first == name.location.view() and std::holds_alternative<CustomTypeSymbol>(pair.second)
                   and (not exported_only or std::get<CustomTypeSymbol>(pair.second).definition->is_exported());
        });
        const auto found = (find_iterator != scope->cend());
        if (found) {
            return std::get<CustomTypeSymbol>(find_iterator->second).definition;
        }
        return nullptr;
    }

    template<>
    [[nodiscard]] const Parser::VariantDefinition*
    get_symbols_in_scope(const Scope* scope, const Parser::Identifier name, bool exported_only) {
        using std::ranges::find_if;
        const auto find_iterator = find_if(*scope, [&](const auto& pair) {
            return pair.first == name.location.view() and std::holds_alternative<StructSymbol>(pair.second)
                   and (not exported_only or std::get<StructSymbol>(pair.second).custom_type_definition->is_exported());
        });
        const auto found = (find_iterator != scope->cend());
        if (found) {
            const auto& struct_symbol = std::get<StructSymbol>(find_iterator->second);
            return &(struct_symbol.custom_type_definition->alternatives.at(struct_symbol.tag));
        }
        return nullptr;
    }

    [[nodiscard]] std::optional<const VariableSymbol*>
    variable_lookup(const Scope* surrounding_scope, const Lexer::Tokens::Identifier& identifier) {
        using std::ranges::find_if;
        while (surrounding_scope != nullptr) {
            const auto find_iterator = find_if(*surrounding_scope, [&](const auto& pair) {
                return pair.first == identifier.location.view() and std::holds_alternative<VariableSymbol>(pair.second);
            });
            const auto found = (find_iterator != surrounding_scope->cend());
            if (found) {
                return &std::get<VariableSymbol>(find_iterator->second);
            }
            surrounding_scope = surrounding_scope->surrounding_scope;
        }
        return {};
    }

    template<typename T>
    [[nodiscard]] T lookup(const Scope* surrounding_scope, const Parser::Expressions::Name& name_expression) {
        using std::ranges::find_if;

        static constexpr auto is_function_lookup = std::same_as<T, std::vector<const FunctionOverload*>>;
        static constexpr auto is_custom_type_lookup = std::same_as<T, const Parser::CustomTypeDefinition*>;
        static constexpr auto is_variant_lookup = std::same_as<T, const Parser::VariantDefinition*>;
        static_assert(is_function_lookup or is_custom_type_lookup or is_variant_lookup);

        /* To do a correct function lookup, we have to start in a scope that corresponds to a namespace. E.g. if
         * the name to be looked up is inside a function, we start in the scope that is owned by the namespace
         * surrounding the name. Example:
         *
         * namespace test {
         *     function f() {
         *         g();
         *     }
         *
         *     function g() { }
         * }
         *
         * In the above snippet, the name g (when invoking the function) occurs in the scope that corresponds to the
         * body of f, but the name lookup has to start in the scope that is owned by the `test`-namespace. */
        surrounding_scope = surrounding_scope->surrounding_namespace->scope.get();

        const auto is_qualified_name = (name_expression.name_tokens.size() > 1);
        auto result = T{};
        const auto name = std::get<Parser::Identifier>(name_expression.name_tokens.back());

        const Scope* current_scope = surrounding_scope;
        if (is_qualified_name) {
            const auto maybe_relative_scope = get_relative_scope(surrounding_scope, name_expression);
            auto found_relative_namespace = static_cast<bool>(maybe_relative_scope);

            if (found_relative_namespace) {
                current_scope = *maybe_relative_scope;
                result = get_symbols_in_scope<T>(current_scope, name, true);
            } else {
                // global namespace lookup fallback
                const auto global_scope = get_global_scope(surrounding_scope);
                const auto maybe_absolute_scope = get_relative_scope(global_scope, name_expression);
                if (maybe_absolute_scope) {
                    /* if the current scope is a child scope of the fallback scope, then the symbol in question
                     * does not have to be exported to be valid */
                    const auto is_child_scope = surrounding_scope->is_child_of(*maybe_absolute_scope);
                    const auto exported_only = not is_child_scope;
                    result = get_symbols_in_scope<T>(*maybe_absolute_scope, name, exported_only);
                }
            }

            const auto nothing_found_yet = (result == T{});
            if (nothing_found_yet) {
                auto error_message = fmt::format("use of undeclared identifier \"{}\"", name.location.view());
                /* Maybe the function in question exists, but is not exported. We do another lookup ignoring whether
                 * the functions are exported or not, and if we find at least one possible overload, we can use this
                 * for a better error message. */
                if (found_relative_namespace) {
                    using std::ranges::views::take, std::ranges::views::transform;
                    current_scope = *maybe_relative_scope;
                    result = get_symbols_in_scope<T>(current_scope, name, false);

                    const auto found_any_symbol = (result != T{});
                    if (found_any_symbol) {
                        if constexpr (is_function_lookup) {
                            error_message += "\nmaybe you're missing an export? possible candidates are:";
                            for (const auto& overload : result | take(3)) {
                                error_message = fmt::format(
                                        "{}\n\t{}{}({})", error_message,
                                        get_absolute_namespace_qualifier(*(overload->surrounding_namespace)),
                                        name.location.view(),
                                        fmt::join(
                                                overload->definition->parameters | transform([](const auto& parameter) {
                                                    return parameter.type_definition->to_string();
                                                }),
                                                ", "
                                        )
                                );
                            }
                            error_message += "\n";
                        }

                        // TODO: better error message for custom types
                    }
                }
                Error::error(name, error_message);
            }
            return result;
        } else {
            while (current_scope != nullptr) {
                const auto find_iterator = find_if(*current_scope, [&](const auto& pair) {
                    if constexpr (is_function_lookup) {
                        return pair.first == name.location.view()
                               and std::holds_alternative<FunctionSymbol>(pair.second);
                    } else if constexpr (is_custom_type_lookup) {
                        return pair.first == name.location.view()
                               and std::holds_alternative<CustomTypeSymbol>(pair.second);
                    } else if constexpr (is_variant_lookup) {
                        return pair.first == name.location.view() and std::holds_alternative<StructSymbol>(pair.second);
                    } else {
                        throw;
                    }
                });
                const auto found = (find_iterator != current_scope->cend());
                if (found) {
                    if constexpr (is_function_lookup) {
                        for (const auto& overload : std::get<FunctionSymbol>(find_iterator->second).overloads) {
                            result.push_back(&overload);
                        }
                        return result;
                    } else if constexpr (is_custom_type_lookup) {
                        return std::get<CustomTypeSymbol>(find_iterator->second).definition;
                    } else if constexpr (is_variant_lookup) {
                        const auto& struct_symbol = std::get<StructSymbol>(find_iterator->second);
                        return &(struct_symbol.custom_type_definition->alternatives[struct_symbol.tag]);
                    } else {
                        throw;
                    }
                }
                current_scope = current_scope->surrounding_scope;
            }
        }
        return result;
    }

    /**
         * Performs a lookup for either a struct of a custom type and, if successful, sets the corresponding
         * member (either struct_definition or custom_type_definition) of the custom type placeholder passed
         * in type_definition to the appropriate definition.
         * If type_definition does not represent a custom type placeholder (e.g. U32), the function returns
         * false. If it represents a custom type placeholder, but the lookup fails, the function issues an
         * error and also returns false.
         * @param type_definition the definition of the type in question
         * @param surrounding_scope the scope the name occurred in
         * @param name_tokens the tokens that form the name expression
         * @return true if the lookup was successful, false otherwise
         */
    static bool custom_type_lookup(DataType* const type_definition, const Scope* const surrounding_scope) {
        // type definition may be a nullptr (when used during auto type-deduction)

        if (type_definition != nullptr) {
            if (type_definition->is_pointer_type()) {
                const auto pointer_type = *(type_definition->as_pointer_type());
                return custom_type_lookup(pointer_type->contained, surrounding_scope);
            }

            if (type_definition->is_function_pointer_type()) {
                const auto function_pointer_type = *(type_definition->as_function_pointer_type());
                bool result = true;
                for (const auto& parameter_type : function_pointer_type->parameter_types) {
                    if (not custom_type_lookup(parameter_type, surrounding_scope)) {
                        result = false;
                    }
                }
                if (not custom_type_lookup(function_pointer_type->return_type, surrounding_scope)) {
                    result = false;
                }
                return result;
            }

            if (type_definition->is_array_type()) {
                const auto array_type = *(type_definition->as_array_type());
                return custom_type_lookup(array_type->contained, surrounding_scope);
            }
        }

        if (type_definition == nullptr or not type_definition->is_custom_type_placeholder()) {
            return false;
        }
        // we know we have a custom type placeholder at hand
        const auto placeholder_type = *(type_definition->as_custom_type_placeholder());
        const auto name_expression = Parser::Expressions::Name{ placeholder_type->type_definition_tokens };
        const auto struct_definition = lookup<const Parser::VariantDefinition*>(surrounding_scope, name_expression);
        const auto struct_type_found = (struct_definition != nullptr);
        if (struct_type_found) {
            placeholder_type->struct_definition = struct_definition;
            return true;
        }
        const auto custom_type_definition =
                lookup<const Parser::CustomTypeDefinition*>(surrounding_scope, name_expression);
        const auto custom_type_definition_found = (custom_type_definition != nullptr);
        if (custom_type_definition_found) {
            placeholder_type->custom_type_definition = custom_type_definition;
            return true;
        }
        using std::ranges::views::transform;
        Error::error(
                placeholder_type->type_definition_tokens.back(),
                fmt::format(
                        "use of undeclared type \"{}\"",
                        fmt::join(
                                name_expression.name_tokens | transform([](const auto& token) {
                                    return Error::token_location(token).view();
                                }),
                                ""
                        )
                )
        );
        return false;
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

            custom_type_lookup(statement.type_definition.get(), statement.surrounding_scope);
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
                        return label_definition->identifier.location.view()
                               == statement.label_identifier.location.view();
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

        void visit(Parser::Expressions::ArrayLiteral& expression) override {
            using Parser::Expressions::Expression;
            expression.surrounding_scope = scope;
            std::visit(
                    overloaded{ [&](const std::vector<std::unique_ptr<Expression>>& values) {
                                   for (const auto& value : values) {
                                       value->accept(*this);
                                   }
                               },
                                [&](const std::pair<std::unique_ptr<Expression>, usize>& pair) {
                                    pair.first->accept(*this);
                                } },
                    expression.values
            );
        }

        void visit(Parser::Expressions::StructLiteral& expression) override {
            using std::ranges::views::transform;

            expression.surrounding_scope = scope;

            // lookup for the type name of the struct literal
            const auto type_definition =
                    lookup<const Parser::VariantDefinition*>(expression.surrounding_scope, expression.type_name);
            if (type_definition == nullptr) {
                Error::error(
                        expression.type_name,
                        fmt::format(
                                "use of undeclared custom type \"{}\"",
                                fmt::join(
                                        expression.type_name.name_tokens | transform([](const auto& token) {
                                            return Error::token_location(token).view();
                                        }),
                                        "::"
                                )
                        )
                );
            }

            expression.definition = type_definition;

            // visit expressions for field values recursively
            for (auto& field : expression.values) {
                field.field_value->accept(*this);
            }
        }

        void visit(Parser::Expressions::Name& expression) override {
            using std::ranges::find_if, Lexer::Tokens::Identifier;
            expression.surrounding_scope = scope;

            const auto maybe_is_variable = (expression.name_tokens.size() == 1);
            const auto name = std::get<Identifier>(expression.name_tokens.back());
            bool found = false;
            if (maybe_is_variable) {
                const auto maybe_variable_symbol = variable_lookup(expression.surrounding_scope, name);
                if (maybe_variable_symbol) {
                    expression.variable_symbol = *maybe_variable_symbol;
                    found = true;
                }
            }
            if (not found) {
                auto overloads = lookup<std::vector<const FunctionOverload*>>(expression.surrounding_scope, expression);
                if (not overloads.empty()) {
                    expression.possible_overloads = std::move(overloads);
                    found = true;
                }
            }

            if (not found) {
                Error::error(name, "use of undeclared identifier");
            }
        }

        void visit(Parser::Expressions::UnaryOperator& expression) override {
            expression.operand->accept(*this);
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            expression.lhs->accept(*this);
            expression.surrounding_scope = scope;

            /* If this is access to a struct attribute, we cannot do a name lookup for the right
             * operand, since we would need to know the type of the struct we want to access.
             * A "real" lookup is not needed anyway. The type checker will later on perform a check if the
             * struct in question really has an attribute with the given name. */
            const auto token = std::get_if<Lexer::Tokens::Token>(&(expression.operator_type)); // maybe nullptr
            const auto is_struct_attribute_access =
                    (token != nullptr and std::holds_alternative<Lexer::Tokens::Dot>(*token));

            if (not is_struct_attribute_access) {
                expression.rhs->accept(*this);
            }
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

    static void visit_function_bodies(Parser::Program& program, TypeContainer& type_container, Scope& global_scope);

    struct ScopeGeneratorBodyVisitor {
        ScopeGeneratorBodyVisitor(TypeContainer* type_container, Scope* surrounding_scope)
            : type_container{ type_container },
              surrounding_scope{ surrounding_scope } { }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        void operator()(std::unique_ptr<Parser::CustomTypeDefinition>&) {
            // TODO: something?
        }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            auto function_scope = surrounding_scope->create_child_scope();
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

                custom_type_lookup(parameter.type_definition.get(), function_scope.get());
            }

            custom_type_lookup(function_definition->return_type_definition.get(), surrounding_scope);

            auto visitor = ScopeGenerator{ function_scope.get(), type_container };
            function_definition->body.scope = std::move(function_scope);
            function_definition->body.accept(visitor);
        }

        void operator()(std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
            visit_function_bodies(namespace_definition->contents, *type_container, *(namespace_definition->scope));
        }

        TypeContainer* type_container;
        Scope* surrounding_scope;
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
        void visit(Parser::Expressions::ArrayLiteral&) override { }
        void visit(Parser::Expressions::StructLiteral&) override { }
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

    static void visit_top_level_statements(Parser::Program& program, TypeContainer& type_container, Scope& scope);

    struct ScopeGeneratorTopLevelVisitor {
        ScopeGeneratorTopLevelVisitor(TypeContainer* type_container, Scope* scope)
            : type_container{ type_container },
              scope{ scope } { }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        void operator()(std::unique_ptr<Parser::CustomTypeDefinition>& type_definition) {
            using namespace std::string_view_literals;
            using std::ranges::find_if;

            if (type_definition->is_exported() and type_definition->namespace_name == "::") {
                Error::error(
                        *(type_definition->export_token), "exporting functions from the global namespace is not allowed"
                );
            }

            auto identifier = type_definition->name.has_value() ? (*(type_definition->name)).location.view() : ""sv;
            auto find_iterator = find_if(*scope, [&](const auto& pair) { return pair.first == identifier; });
            const auto found = (find_iterator != std::end(*scope));
            if (found) {
                Error::error(*(type_definition->name), fmt::format("redefinition of identifier \"{}\"", identifier));
            }
            (*scope)[identifier] = CustomTypeSymbol{ .definition{ type_definition.get() } };

            type_definition->surrounding_scope = scope;

            /* the definition of a custom type opens a new scope that holds the symbols of the
             * type variants */
            type_definition->inner_scope = scope->create_child_scope();
            for (const auto& alternative : type_definition->alternatives) {
                if (type_definition->inner_scope->contains(alternative.second.name.location.view())) {
                    Error::error(
                            alternative.second.name, fmt::format(
                                                             "duplicate alternative \"{}\" in custom type \"{}\"",
                                                             alternative.second.name.location.view(), identifier
                                                     )
                    );
                }
                (*(type_definition->inner_scope))[alternative.second.name.location.view()] =
                        StructSymbol{ .custom_type_definition{ type_definition.get() }, .tag{ alternative.first } };
            }

            if (not type_definition->is_restricted()) {
                /* for non-restricted custom type, we have to expose the alternatives to the surrounding namespace
                 * to make them available there */
                for (const auto& alternative : type_definition->alternatives) {
                    if (scope->contains(alternative.second.name.location.view())) {
                        Error::error(
                                alternative.second.name,
                                fmt::format(
                                        "redefinition of identifier \"{}\" in surrounding scope of custom type \"{}\"\n"
                                        "are you missing the \"restricted\" keyword?",
                                        alternative.second.name.location.view(), identifier
                                )
                        );
                    }
                    (*scope)[alternative.second.name.location.view()] =
                            StructSymbol{ .custom_type_definition{ type_definition.get() }, .tag{ alternative.first } };
                }
            }
        }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using namespace std::string_literals;
            using std::ranges::find_if;

            if (function_definition->export_token.has_value() and scope->surrounding_scope == nullptr) {
                Error::error(
                        *(function_definition->export_token),
                        "exporting functions from the global namespace is not allowed"
                );
            }

            auto identifier = function_definition->name.location.view();
            auto find_iterator = find_if(*scope, [&](const auto& pair) { return pair.first == identifier; });
            const auto found = (find_iterator != std::end(*scope));
            auto function_overload = FunctionOverload{ .surrounding_namespace{ scope->surrounding_namespace },
                                                       .definition{ function_definition.get() } };

            if (found) {
                if (not std::holds_alternative<FunctionSymbol>(find_iterator->second)) {
                    Error::error(
                            function_definition->name,
                            fmt::format("redefinition of identifier \"{}\"", function_definition->name.location.view())
                    );
                }
                auto& function_symbol = std::get<FunctionSymbol>(find_iterator->second);
                auto& new_overload = function_symbol.overloads.emplace_back(std::move(function_overload));
                function_definition->corresponding_symbol = &new_overload;
                assert(std::get<FunctionSymbol>((*scope)[identifier]).overloads.size() > 1);
            } else {
                auto new_symbol = FunctionSymbol{ .overloads{ { std::move(function_overload) } } };
                function_definition->corresponding_symbol = &new_symbol.overloads.back();
                (*scope)[identifier] = std::move(new_symbol);
            }

            auto label_visitor = ScopeGeneratorLabelVisitor{ function_definition.get() };
            function_definition->body.accept(label_visitor);
            function_definition->surrounding_scope = scope;
        }

        void operator()(std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
            using std::ranges::find_if;

            /* The parser already merged all namespaces with the same name. If we encounter a namespace definition
             * we can be sure that it's the only one with the given name. If the symbol table of the current scope
             * already contains a symbol with the same, this cannot be a namespace name (e.g. it may be a function
             * symbol) and this name cannot be used for the namespace. */
            const auto namespace_name = namespace_definition->name.location.view();
            const auto find_iterator = find_if(*scope, [&](const auto& pair) {
                const auto is_same_name = (pair.first == namespace_name);
                if (is_same_name) {
                    assert(not std::holds_alternative<NamespaceSymbol>(pair.second));
                }
                return is_same_name;
            });
            const auto found = (find_iterator != scope->cend());

            if (found) {
                Error::error(
                        namespace_definition->name, fmt::format("redefinition of identifier \"{}\"", namespace_name)
                );
            }

            /* If we are in the global namespace, we do not open a new child scope for this namespace, because
             * it has already been opened inside the main function. */
            const auto is_global_namespace = namespace_name.empty();
            if (not is_global_namespace) {
                (*scope)[namespace_name] = NamespaceSymbol{ .namespace_definition{ namespace_definition.get() } };
                namespace_definition->scope = scope->create_child_scope();
                namespace_definition->scope->surrounding_namespace = namespace_definition.get();
            }

            visit_top_level_statements(namespace_definition->contents, *type_container, *(namespace_definition->scope));
        }

        TypeContainer* type_container;
        Scope* scope;
    };

    void visit_top_level_statements(Parser::Program& program, TypeContainer& type_container, Scope& scope) {
        auto visitor = ScopeGeneratorTopLevelVisitor{ &type_container, &scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    static void visit_function_bodies(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        auto visitor = ScopeGeneratorBodyVisitor{ &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    void generate(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        visit_top_level_statements(program, type_container, global_scope);
        visit_function_bodies(program, type_container, global_scope);
    }

} // namespace ScopeGenerator
