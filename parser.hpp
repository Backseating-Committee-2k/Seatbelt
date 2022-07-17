//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include "data_type.hpp"
#include "lexer.hpp"
#include "parameter_list.hpp"
#include "scope.hpp"
#include <cassert>
#include <iostream>
#include <memory>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

template<class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

namespace Parser {
    using namespace Lexer::Tokens;

    struct FunctionDefinition;

    namespace Expressions {
        struct Expression;
    }

    namespace Statements {

        using Expressions::Expression;

        struct Block;
        struct VariableDefinition;
        struct InlineAssembly;
        struct ExpressionStatement;

        struct StatementVisitor {
            virtual void visit(Block& statement) = 0;
            virtual void visit(VariableDefinition& statement) = 0;
            virtual void visit(InlineAssembly& statement) = 0;
            virtual void visit(ExpressionStatement& statement) = 0;

            virtual ~StatementVisitor() = default;
        };

        struct Statement {
            virtual ~Statement() = default;

            virtual void accept(StatementVisitor& visitor) = 0;

            const Scope* surrounding_scope{ nullptr };
        };

        using StatementList = std::vector<std::unique_ptr<Statement>>;

        struct Block : public Statement {
            explicit Block(StatementList statements) : statements{ std::move(statements) } { }

            void accept(StatementVisitor& visitor) override {
                visitor.visit(*this);
            }

            StatementList statements;
            std::unique_ptr<Scope> scope;
        };

        struct VariableDefinition : public Statement {
            explicit VariableDefinition(
                    Identifier name,
                    Equals equals_token,
                    std::span<const Token> type_tokens,
                    std::unique_ptr<Expression> initial_value
            )
                : name{ name },
                  equals_token{ equals_token },
                  type_tokens{ type_tokens },
                  initial_value{ std::move(initial_value) } { }

            void accept(StatementVisitor& visitor) override {
                visitor.visit(*this);
            }

            Identifier name;
            Equals equals_token;
            std::span<const Token> type_tokens;
            const DataType* type{ nullptr };
            std::unique_ptr<Expression> initial_value;
        };

        struct InlineAssembly : public Statement {
            explicit InlineAssembly(const Lexer::Tokens::InlineAssembly* token) : token{ token } { }

            void accept(StatementVisitor& visitor) override {
                visitor.visit(*this);
            }

            const Lexer::Tokens::InlineAssembly* token;
        };

        struct ExpressionStatement : public Statement {
            explicit ExpressionStatement(std::unique_ptr<Expression> expression)
                : expression{ std::move(expression) } { }

            void accept(StatementVisitor& visitor) override {
                visitor.visit(*this);
            }

            std::unique_ptr<Expression> expression;
        };

    }// namespace Statements

    namespace Expressions {

        struct Integer;
        struct Char;
        struct Name;
        struct BinaryOperator;
        struct FunctionCall;

        struct ExpressionVisitor {
            virtual void visit(Integer& expression) = 0;
            virtual void visit(Char& expression) = 0;
            virtual void visit(Name& expression) = 0;
            virtual void visit(BinaryOperator& expression) = 0;
            virtual void visit(FunctionCall& expression) = 0;

            virtual ~ExpressionVisitor() = default;
        };

        struct Expression {
            virtual ~Expression() = default;
            virtual void accept(ExpressionVisitor& visitor) = 0;

            const DataType* data_type{ nullptr };
            const Scope* surrounding_scope{ nullptr };
        };

        template<typename T>
        struct ExpressionAcceptor : public Expression {
            using Expression::Expression;

            void accept(ExpressionVisitor& visitor) final {
                visitor.visit(static_cast<T&>(*this));
            }
        };

        struct Integer : public ExpressionAcceptor<Integer> {
            explicit Integer(IntegerLiteral value) : value{ value } { }

            IntegerLiteral value;
        };

        struct Char : public ExpressionAcceptor<Char> {
            explicit Char(CharLiteral value) : value{ value } { }

            CharLiteral value;
        };

        struct Name : public ExpressionAcceptor<Name> {
            explicit Name(std::span<const Lexer::Tokens::Token> name_tokens) : name_tokens{ name_tokens } { }

            std::span<const Lexer::Tokens::Token> name_tokens;
            const DataType* definition_data_type{ nullptr };
            std::optional<std::vector<const FunctionOverload*>> possible_overloads{};
        };

        struct BinaryOperator : public ExpressionAcceptor<BinaryOperator> {
            BinaryOperator(std::unique_ptr<Expression> lhs, std::unique_ptr<Expression> rhs, Token operator_token)
                : lhs{ std::move(lhs) },
                  rhs{ std::move(rhs) },
                  operator_token{ operator_token } { }

            std::unique_ptr<Expression> lhs;
            std::unique_ptr<Expression> rhs;
            Token operator_token;
        };

        struct FunctionCall : public ExpressionAcceptor<FunctionCall> {
            FunctionCall(std::unique_ptr<Expression> callee, std::vector<std::unique_ptr<Expression>> arguments)
                : callee{ std::move(callee) },
                  arguments{ std::move(arguments) } { }

            std::unique_ptr<Expression> callee;
            std::vector<std::unique_ptr<Expression>> arguments;
        };

    }// namespace Expressions

    struct FunctionDefinition {
        Identifier name;
        ParameterList parameters;
        std::span<const Token> return_type_tokens;
        const DataType* return_type{ nullptr };
        Statements::Block body;
        FunctionOverload* corresponding_symbol{ nullptr };
        std::string namespace_name{};
    };

    struct ImportStatement {
        Import import_token;
        std::span<const Token> import_path_tokens;
    };

    template<typename... T>
    using PointerVariant = std::variant<std::unique_ptr<T>...>;

    using Program = std::vector<PointerVariant<FunctionDefinition, ImportStatement>>;

    void concatenate_programs(Program& first, Program&& second);

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens);

}// namespace Parser
