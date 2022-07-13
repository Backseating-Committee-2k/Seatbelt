//
// Created by coder2k on 10.07.2022.
//

#include "type_checker.hpp"
#include "error.hpp"
#include <fmt/core.h>
#include <cassert>
#include <variant>
#include <string_view>
#include <sstream>

namespace TypeChecker {
    static constexpr std::string_view U32Identifier{ "U32" };

    template<typename... Types>
    [[nodiscard]] static bool holds_any_of(const Lexer::Tokens::Token& token) {
        return (std::holds_alternative<Types>(token) or ...);
    }

    [[nodiscard]] DataTypePointer token_to_type(const Lexer::Tokens::Token token) {
        return std::make_unique<ConcreteType>(Error::token_location(token).view(), false);
    }

    [[nodiscard]] DataTypePointer tokens_to_type(const std::span<const Lexer::Tokens::Token> tokens) {
        auto result = token_to_type(tokens.back());
        for (auto iterator = std::crbegin(tokens) + 1; iterator != std::crend(tokens); ++iterator) {
            if (not std::holds_alternative<Lexer::Tokens::Arrow>(*iterator)) {
                Error::error(*iterator, "invalid type specifier");
            }
            result = std::make_unique<PointerType>(std::move(result), false);
        }
        return result;
    }

    struct TypeCheckerVisitor : public Parser::Statements::StatementVisitor,
                                public Parser::Expressions::ExpressionVisitor {
        void visit(Parser::Statements::Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                statement.accept(*this);
            }
        }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            statement.type = tokens_to_type(statement.type_tokens);
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
            expression.data_type = std::make_unique<ConcreteType>(U32Identifier, false);
        }

        void visit(Parser::Expressions::Name& expression) override {
            expression.data_type = expression.surrounding_scope->at(expression.name.location.view()).data_type->clone();
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            using namespace Lexer::Tokens;
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            if (const auto concrete_type = dynamic_cast<const ConcreteType*>(expression.lhs->data_type.get())) {
                if (holds_any_of<Plus, Minus, Asterisk, ForwardSlash>(expression.operator_token) and
                    concrete_type->name == U32Identifier) {
                    expression.data_type = concrete_type->clone();
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

        void visit(Parser::Expressions::FunctionCall& expression) override { }
    };

    struct TypeCheckerTopLevelVisitor {
        explicit TypeCheckerTopLevelVisitor(const Parser::Program* program) : program{ program } { }

        void operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) const {
            for (const auto& statement : function_definition->body.statements) {
                auto visitor = TypeCheckerVisitor{};
                statement->accept(visitor);
            }
        }

        const Parser::Program* program;
    };

    void check(const Parser::Program& program) {
        auto visitor = TypeCheckerTopLevelVisitor{ &program };
        for (const auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }
}// namespace TypeChecker
