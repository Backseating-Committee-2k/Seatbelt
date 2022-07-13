//
// Created by coder2k on 10.07.2022.
//

#include "type_checker.hpp"
#include "error.hpp"
#include <cassert>
#include <fmt/core.h>
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

        void visit(Parser::Expressions::FunctionCall& expression) override { }

        TypeContainer* type_container;
    };

    struct TypeCheckerTopLevelVisitor {
        TypeCheckerTopLevelVisitor(const Parser::Program* program, TypeContainer* type_container)
            : program{ program },
              type_container{ type_container } { }

        void operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) const {
            for (const auto& statement : function_definition->body.statements) {
                auto visitor = TypeCheckerVisitor{ type_container };
                statement->accept(visitor);
            }
        }

        const Parser::Program* program;
        TypeContainer* type_container;
    };

    void check(const Parser::Program& program, TypeContainer& type_container) {
        auto visitor = TypeCheckerTopLevelVisitor{ &program, &type_container };
        for (const auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }
}// namespace TypeChecker
