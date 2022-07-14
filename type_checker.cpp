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

        void visit(Parser::Statements::InlineAssembly& statement) override { }

        void visit(Parser::Statements::ExpressionStatement& statement) override {
            statement.expression->accept(*this);
        }

        void visit(Parser::Expressions::Literal& expression) override {
            expression.data_type = type_container->from_data_type(std::make_unique<ConcreteType>(U32Identifier, false));
        }

        void visit(Parser::Expressions::Name& expression) override {
            // the scope generator should fill this value beforehand
            assert(expression.definition_data_type != nullptr && "unreachable (use of undeclared identifier)");
            auto current_scope = expression.surrounding_scope;
            const auto identifier = expression.name.location.view();
            while (current_scope != nullptr) {
                const auto find_iterator = current_scope->find(identifier);
                const auto found = find_iterator != std::cend(*current_scope);
                if (not found) {
                    goto end_of_loop;
                }
                if (const auto function_symbol = std::get_if<FunctionSymbol>(&find_iterator->second)) { }
            end_of_loop:
                current_scope = current_scope->surrounding_scope;
            }
            expression.data_type = expression.definition_data_type;
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
                const auto identifier = name->name.location.view();
                auto signature = fmt::format("${}", identifier);
                for (const auto& argument : expression.arguments) {
                    signature += argument->data_type->mangled_name();
                }
                auto current_scope = expression.surrounding_scope;
                while (current_scope != nullptr) {
                    fmt::print(stderr, "contents of current scope:\n");
                    for (const auto& [key, _] : *current_scope) {
                        fmt::print(stderr, "\t{}\n", key);
                    }
                    const auto find_iterator = current_scope->find(identifier);
                    const auto found = find_iterator != std::cend(*current_scope);
                    if (found) {
                        if (const auto function_symbol = std::get_if<FunctionSymbol>(&find_iterator->second)) {
                            fmt::print(stderr, "trying to find function overload for {}\n", identifier);
                            const auto& overloads = function_symbol->overloads;
                            const auto overload_iterator = find_if(overloads, [&](const auto& overload) {
                                fmt::print(stderr, "\tcomparing {} with {}\n", overload.signature, signature);
                                return overload.signature == signature;
                            });
                            const auto overload_found = overload_iterator != std::cend(overloads);
                            if (overload_found) {
                                const auto& overload = *overload_iterator;
                                assert(overload.return_type != nullptr && "return type has to be set before");
                                name->data_type = type_container->from_data_type(
                                        std::make_unique<FunctionPointerType>(signature, false)
                                );
                                fmt::print(stderr, "found function overload!\n");
                            } else {
                                Error::error(name->name, "no matching function overload found");
                            }
                        } else {
                            // TODO: delete this error and check (later) if this is a function pointer
                            Error::error(name->name, fmt::format("the value of \"{}\" is not callable", identifier));
                        }
                    }
                    current_scope = current_scope->surrounding_scope;
                }
            }
        }

        TypeContainer* type_container;
    };

    struct TypeCheckerTopLevelVisitor {
        TypeCheckerTopLevelVisitor(const Parser::Program* program, TypeContainer* type_container)
            : program{ program },
              type_container{ type_container } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            auto signature = fmt::format("${}", function_definition->name.location.view());
            for (auto& parameter : function_definition->parameters) {
                parameter.type = type_container->from_tokens(parameter.type_tokens);
                signature += parameter.type->mangled_name();
            }

            function_definition->corresponding_symbol->signature = std::move(signature);

            function_definition->return_type = type_container->from_tokens(function_definition->return_type_tokens);
            function_definition->corresponding_symbol->return_type = function_definition->return_type;

            auto visitor = TypeCheckerVisitor{ type_container };
            for (const auto& statement : function_definition->body.statements) {
                statement->accept(visitor);
            }
        }

        const Parser::Program* program;
        TypeContainer* type_container;
    };

    void check(Parser::Program& program, TypeContainer& type_container) {
        auto visitor = TypeCheckerTopLevelVisitor{ &program, &type_container };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }
}// namespace TypeChecker
