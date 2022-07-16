//
// Created by coder2k on 10.07.2022.
//

#include "type_checker.hpp"
#include "error.hpp"
#include <cassert>
#include <fmt/core.h>
#include <ranges>
#include <string_view>
#include <variant>

namespace TypeChecker {
    static constexpr std::string_view U32Identifier{ "U32" };

    template<typename... Types>
    [[nodiscard]] static bool holds_any_of(const Lexer::Tokens::Token& token) {
        return (std::holds_alternative<Types>(token) or ...);
    }

    struct TypeCheckerVisitor : public Parser::Statements::StatementVisitor,
                                public Parser::Expressions::ExpressionVisitor {
        explicit TypeCheckerVisitor(TypeContainer* type_container) : type_container{ type_container } { }

        void visit(Parser::Statements::Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
        }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            statement.type = type_container->from_tokens(statement.type_tokens);
            statement.initial_value->accept(*this);
            assert(statement.type and statement.initial_value->data_type and "missing type information");

            if (statement.type != statement.initial_value->data_type) {
                Error::error(
                        statement.equals_token,
                        fmt::format(
                                R"(incompatible types "{}" and "{}")", statement.type->to_string(),
                                statement.initial_value->data_type->to_string()
                        )
                );
            }
        }

        void visit(Parser::Statements::InlineAssembly&) override { }

        void visit(Parser::Statements::ExpressionStatement& statement) override {
            statement.expression->accept(*this);
        }

        void visit(Parser::Expressions::Literal& expression) override {
            expression.data_type = type_container->from_data_type(std::make_unique<ConcreteType>(U32Identifier, false));
        }

        void visit(Parser::Expressions::Name& expression) override {
            /*  The scope generator fills in the data types of variable definitions beforehand.
             *  But for function pointers we do not have any type information available and
             *  have to fetch the data here. */
            const auto is_variable = expression.definition_data_type != nullptr;
            if (is_variable) {
                expression.data_type = expression.definition_data_type;
            } else {
                // only the last token of a qualified name is relevant for lookup
                const auto& name_token = expression.name_tokens.back();
                const auto identifier = Error::token_location(name_token).view();
                const auto symbol = scope_lookup(expression.surrounding_scope, identifier);
                if (symbol == nullptr) {
                    Error::error(name_token, fmt::format("use of undeclared identifier \"{}\"", identifier));
                }
                if (const auto function_symbol = std::get_if<FunctionSymbol>(symbol)) {
                    const auto& overloads = function_symbol->overloads;
                    assert(not overloads.empty() && "there shall never be a function with zero overloads");
                    if (overloads.size() > 1) {
                        Error::error(name_token, fmt::format("use of identifier \"{}\" is ambiguous", identifier));
                    }
                    assert(overloads.size() == 1);
                    expression.data_type = type_container->from_data_type(
                            std::make_unique<FunctionPointerType>(overloads.front().signature, false)
                    );
                } else {
                    assert(false && "unreachable");
                }
            }
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            using namespace Lexer::Tokens;
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            if (const auto concrete_type = dynamic_cast<const ConcreteType*>(expression.lhs->data_type)) {
                if (holds_any_of<Plus, Minus, Asterisk, ForwardSlash>(expression.operator_token) and
                    concrete_type->name == U32Identifier) {
                    expression.data_type = concrete_type;
                    return;
                }
            }
            Error::error(
                    expression.operator_token,
                    fmt::format(
                            R"(operator "{}" can not be applied to operands of type "{}" and "{}")",
                            Error::token_location(expression.operator_token).view(),
                            expression.lhs->data_type->to_string(), expression.rhs->data_type->to_string()
                    )
            );
        }

        void visit(Parser::Expressions::FunctionCall& expression) override {
            using std::ranges::find_if;

            for (const auto& argument : expression.arguments) {
                argument->accept(*this);
            }

            if (const auto name = dynamic_cast<Parser::Expressions::Name*>(expression.callee.get())) {
                if (name->possible_overloads.has_value()) {
                    auto& possible_overloads = name->possible_overloads.value();
                    const auto& name_token = name->name_tokens.back();
                    const auto identifier = Error::token_location(name_token).view();
                    auto signature = fmt::format("${}", identifier);
                    for (const auto& argument : expression.arguments) {
                        signature += argument->data_type->mangled_name();
                    }
                    bool overload_found = false;
                    for (const auto& overload : possible_overloads) {
                        if (overload->signature == signature) {
                            assert(overload->return_type != nullptr && "return type has to be set before");
                            name->data_type = type_container->from_data_type(
                                    std::make_unique<FunctionPointerType>(signature, false)
                            );
                            overload_found = true;
                        }
                    }
                    if (not overload_found) {
                        Error::error(name_token, fmt::format("no matching function overload found", identifier));
                    }
                    // erase all possible overloads with the wrong signature
                    possible_overloads.erase(
                            std::remove_if(
                                    std::begin(possible_overloads), std::end(possible_overloads),
                                    [&](const auto& overload) { return overload->signature != signature; }
                            ),
                            std::end(possible_overloads)
                    );
                    // erase all overloads except for the last one (which is the "inner" one)
                    possible_overloads.erase(std::begin(possible_overloads), std::end(possible_overloads) - 1);
                    assert(possible_overloads.size() == 1);
                } else {
                    // this is a function pointer
                    assert(false && "not implemented");
                }
            }
        }

        TypeContainer* type_container;
    };

    struct TypeCheckerTopLevelVisitor {
        TypeCheckerTopLevelVisitor(
                const Parser::Program* program,
                TypeContainer* type_container,
                const Scope* global_scope
        )
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            // the actual signature of the function must have been visited before, we now
            // only visit the body
            auto visitor = TypeCheckerVisitor{ type_container };
            for (const auto& statement : function_definition->body.statements) {
                statement->accept(visitor);
            }
        }

        void operator()(std::unique_ptr<Parser::ImportStatement>& import_statement) { }

        const Parser::Program* program;
        TypeContainer* type_container;
        const Scope* global_scope;
    };

    struct FunctionDefinitionVisitor {
        FunctionDefinitionVisitor(TypeContainer* type_container, const Scope* global_scope)
            : type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using std::ranges::find_if;

            auto signature = fmt::format("${}", function_definition->name.location.view());
            for (auto& parameter : function_definition->parameters) {
                parameter.type = type_container->from_tokens(parameter.type_tokens);
                signature += parameter.type->mangled_name();
            }

            const auto identifier = function_definition->name.location.view();

            const auto find_iterator = global_scope->find(identifier);
            const auto found = (find_iterator != std::cend(*global_scope));
            assert(found && "scope generator should have put the needed symbol into scope or throw an error");
            const auto& found_symbol = find_iterator->second;
            assert(std::holds_alternative<FunctionSymbol>(found_symbol));
            const auto& function_symbol = std::get<FunctionSymbol>(found_symbol);
            const auto& overloads = function_symbol.overloads;

            for (const auto& overload : overloads) {
                const auto duplicate_signature = (overload.signature == signature);
                if (duplicate_signature) {
                    const auto same_namespace = (overload.namespace_name == function_definition->namespace_name);
                    if (same_namespace) {
                        Error::error(
                                function_definition->name,
                                fmt::format("function overload for \"{}\" with ambiguous signature", identifier)
                        );
                    }
                }
            }

            /*const auto find_signature_iterator =
                    find_if(overloads, [&](const auto& overload) { return overload.signature == signature; });
            const auto duplicate_signature = (find_signature_iterator != std::cend(overloads));
            fmt::print(stderr, "duplicate signature found for {}? {}\n", identifier, duplicate_signature);
            fmt::print(stderr, "there are {} functions called {}\n", overloads.size(), identifier);*/


            function_definition->corresponding_symbol->signature = std::move(signature);

            function_definition->return_type = type_container->from_tokens(function_definition->return_type_tokens);
            function_definition->corresponding_symbol->return_type = function_definition->return_type;

            // the body of the function is not recursively visited here since we have to first visit
            // all function signatures before visiting the bodies
        }

        void operator()(auto&) { }

        TypeContainer* type_container;
        const Scope* global_scope;
    };

    void
    visit_function_definitions(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        auto visitor = FunctionDefinitionVisitor{ &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    void check(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        visit_function_definitions(program, type_container, global_scope);

        auto visitor = TypeCheckerTopLevelVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }
}// namespace TypeChecker