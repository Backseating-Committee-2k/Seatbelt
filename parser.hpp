//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include <vector>
#include <variant>
#include <span>
#include <memory>
#include "lexer.hpp"

namespace Parser {

    using namespace Lexer::Tokens;

    using Type = std::span<const Token>;

    namespace Expressions {
        struct Expression;
    }

    namespace Statements {

        using Expressions::Expression;

        struct Statement {
            virtual ~Statement() = default;
        };

        using StatementList = std::vector<std::unique_ptr<Statement>>;

        struct Block : public Statement {
            explicit Block(StatementList statements) : statements{ std::move(statements) } { }

            StatementList statements;
        };

        struct VariableDefinition : public Statement {
            VariableDefinition(Identifier name, Type type, std::unique_ptr<Expression> initial_value)
                : name{ name },
                  type{ type },
                  initial_value{ std::move(initial_value) } { }

            Identifier name;
            Type type;
            std::unique_ptr<Expression> initial_value;
        };

    }// namespace Statements

    namespace Expressions {
        struct Expression {
            virtual ~Expression() = default;
        };

        struct Literal : public Expression {
            explicit Literal(IntegerLiteral value) : value{ value } { }

            IntegerLiteral value;
        };

        struct Name : public Expression {
            explicit Name(Lexer::Tokens::Identifier name) : name{ name } { }

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
        };

        struct Multiplication : public BinaryOperator {
            using BinaryOperator::BinaryOperator;
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

    using Program = std::vector<std::variant<FunctionDefinition>>;

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens);

}// namespace Parser
