//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include <vector>
#include <variant>
#include <span>
#include <memory>
#include <unordered_map>
#include "lexer.hpp"

namespace Parser {
    struct FunctionDefinition;
}

namespace Statements {
    struct Statement;
}

namespace Parser {

    using namespace Lexer::Tokens;

    using Type = std::span<const Token>;

    namespace Expressions {
        struct Expression;
    }

    namespace Statements {

        using Expressions::Expression;

        struct Block;
        struct VariableDefinition;

        struct StatementVisitor {
            virtual void visit(Block& statement) = 0;
            virtual void visit(VariableDefinition& statement) = 0;

            virtual ~StatementVisitor() = default;
        };

        struct Statement {
            virtual void accept(StatementVisitor& visitor) = 0;

            virtual ~Statement() = default;
        };

        using StatementList = std::vector<std::unique_ptr<Statement>>;

        struct Block : public Statement {
            explicit Block(StatementList statements) : statements{ std::move(statements) } { }

            void accept(StatementVisitor& visitor) override {
                visitor.visit(*this);
            }

            StatementList statements;
        };

        struct VariableDefinition : public Statement {
            VariableDefinition(Identifier name, Type type, std::unique_ptr<Expression> initial_value)
                : name{ name },
                  type{ type },
                  initial_value{ std::move(initial_value) } { }

            void accept(StatementVisitor& visitor) override {
                visitor.visit(*this);
            }

            Identifier name;
            Type type;
            std::unique_ptr<Expression> initial_value;
        };

    }// namespace Statements

    namespace Expressions {

        struct Literal;
        struct Name;
        struct Addition;
        struct Subtraction;
        struct Multiplication;
        struct Division;

        struct ExpressionVisitor {
            virtual void visit(Literal& expression) = 0;
            virtual void visit(Name& expression) = 0;
            virtual void visit(Addition& expression) = 0;
            virtual void visit(Subtraction& expression) = 0;
            virtual void visit(Multiplication& expression) = 0;
            virtual void visit(Division& expression) = 0;

            virtual ~ExpressionVisitor() = default;
        };

        struct Expression {
            virtual void accept(ExpressionVisitor& visitor) = 0;

            virtual ~Expression() = default;
        };

        struct Literal : public Expression {
            explicit Literal(IntegerLiteral value) : value{ value } { }

            void accept(ExpressionVisitor& visitor) override {
                visitor.visit(*this);
            }

            IntegerLiteral value;
        };

        struct Name : public Expression {
            explicit Name(Lexer::Tokens::Identifier name) : name{ name } { }

            void accept(ExpressionVisitor& visitor) override {
                visitor.visit(*this);
            }

            Lexer::Tokens::Identifier name;
        };

        struct BinaryOperator : public Expression {
            BinaryOperator(std::unique_ptr<Expression> lhs, std::unique_ptr<Expression> rhs)
                : lhs{ std::move(lhs) },
                  rhs{ std::move(rhs) } { }

            std::unique_ptr<Expression> lhs;
            std::unique_ptr<Expression> rhs;
        };

        struct Addition : public BinaryOperator {
            using BinaryOperator::BinaryOperator;

            void accept(ExpressionVisitor& visitor) override {
                visitor.visit(*this);
            }
        };

        struct Subtraction : public BinaryOperator {
            using BinaryOperator::BinaryOperator;

            void accept(ExpressionVisitor& visitor) override {
                visitor.visit(*this);
            }
        };

        struct Multiplication : public BinaryOperator {
            using BinaryOperator::BinaryOperator;

            void accept(ExpressionVisitor& visitor) override {
                visitor.visit(*this);
            }
        };

        struct Division : public BinaryOperator {
            using BinaryOperator::BinaryOperator;

            void accept(ExpressionVisitor& visitor) override {
                visitor.visit(*this);
            }
        };

    }// namespace Expressions

    struct Parameter {
        Identifier name;
        Type type;
    };

    using ParameterList = std::vector<Parameter>;

    struct FunctionDefinition {
        Identifier name;
        ParameterList parameters;
        Type return_type;
        Statements::Block body;
    };

    template<typename... T>
    using PointerVariant = std::variant<std::unique_ptr<T>...>;

    using Program = std::vector<PointerVariant<FunctionDefinition>>;

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens);

}// namespace Parser
