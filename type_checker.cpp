//
// Created by coder2k on 10.07.2022.
//

#include "type_checker.hpp"
#include "error.hpp"
#include <cassert>
#include <variant>
#include <string_view>
#include <format>
#include <sstream>

namespace TypeChecker {
    static constexpr std::string_view U32Identifier{ "U32" };

    [[nodiscard]] Parser::DataTypePointer token_to_type(const Lexer::Tokens::Token token) {
        return std::make_unique<Parser::ConcreteType>(Error::token_location(token).view(), false);
    }

    [[nodiscard]] Parser::DataTypePointer tokens_to_type(const std::span<const Lexer::Tokens::Token> tokens) {
        auto result = token_to_type(tokens.back());
        for (auto iterator = std::crbegin(tokens) + 1; iterator != std::crend(tokens); ++iterator) {
            if (not std::holds_alternative<Lexer::Tokens::Arrow>(*iterator)) {
                Error::error(*iterator, "invalid type specifier");
            }
            result = std::make_unique<Parser::PointerType>(std::move(result), false);
        }
        return result;
    }

    struct TypeCheckerVisitor : public Parser::Statements::StatementVisitor,
                                public Parser::Expressions::ExpressionVisitor {
        void visit(Parser::Statements::Block& statement) override { }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            statement.type = tokens_to_type(statement.type_tokens);
            statement.initial_value->accept(*this);
            assert(statement.type and statement.initial_value->data_type and "missing type information");

            if (statement.type != statement.initial_value->data_type) {
                Error::error(
                        statement.equals_token,
                        std::format(
                                R"(incompatible types "{}" and "{}")", statement.type->to_string(),
                                statement.initial_value->data_type->to_string()
                        )
                );
            }
        }

        void visit(Parser::Statements::InlineAssembly& statement) override { }

        void visit(Parser::Statements::ExpressionStatement& statement) override { }

        void visit(Parser::Expressions::Literal& expression) override {
            expression.data_type = std::make_unique<Parser::ConcreteType>(U32Identifier, false);
        }

        void visit(Parser::Expressions::Name& expression) override {

        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            if (const auto concrete_type = dynamic_cast<const Parser::ConcreteType*>(expression.lhs->data_type.get())) {
                if ((std::holds_alternative<Lexer::Tokens::Plus>(expression.operator_token) or
                     std::holds_alternative<Lexer::Tokens::Minus>(expression.operator_token) or
                     std::holds_alternative<Lexer::Tokens::Asterisk>(expression.operator_token) or
                     std::holds_alternative<Lexer::Tokens::ForwardSlash>(expression.operator_token)) and
                    concrete_type->name == U32Identifier) {
                    expression.data_type = std::make_unique<Parser::ConcreteType>(*concrete_type);
                    return;
                }
            }
            Error::error(
                    expression.operator_token, std::format(
                                                       R"(operator "{}" can not be applied to operands of type "{}")",
                                                       Error::token_location(expression.operator_token).view(),
                                                       expression.lhs->data_type->to_string()
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
