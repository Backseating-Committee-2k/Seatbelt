//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include <cassert>
#include <vector>
#include <variant>
#include <span>
#include <memory>
#include <unordered_map>
#include <optional>
#include <string_view>
#include <iostream>
#include "lexer.hpp"

namespace Parser {
    struct FunctionDefinition;
}

namespace Statements {
    struct Statement;
}

namespace Parser {

    using namespace Lexer::Tokens;

    namespace Expressions {
        struct Expression;
    }

    struct DataType {
    protected:
        explicit DataType(bool is_mutable) : is_mutable{ is_mutable } { }

    public:
        virtual ~DataType() = default;

        virtual bool operator==(const DataType& other) const {
            return is_mutable == other.is_mutable;
        }

        virtual std::string to_string() const {
            return is_mutable ? "mutable " : "const ";
        }

        bool is_mutable;
    };

    struct ConcreteType : public DataType {
        ConcreteType(std::string_view name, bool is_mutable) : DataType{ is_mutable }, name{ std::move(name) } { }

        bool operator==(const DataType& other) const override {
            if (const auto other_pointer = dynamic_cast<const ConcreteType*>(&other)) {
                return DataType::operator==(other) and name == other_pointer->name;
            }
            return false;
        }

        std::string to_string() const override {
            return DataType::to_string() + std::string{ name };
        }

        std::string_view name;
    };

    using DataTypePointer = std::unique_ptr<DataType>;

    struct PointerType : public DataType {
        PointerType(std::unique_ptr<DataType> contained, bool is_mutable)
            : DataType{ is_mutable },
              contained{ std::move(contained) } { }

        bool operator==(const DataType& other) const override {
            if (const auto other_pointer = dynamic_cast<const PointerType*>(&other)) {
                return DataType::operator==(other) and contained == other_pointer->contained;
            }
            return false;
        }

        std::string to_string() const override {
            using namespace std::string_literals;
            assert(contained);
            return DataType::to_string() + "->"s + contained->to_string();
        }

        std::unique_ptr<DataType> contained;
    };

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
            VariableDefinition(
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
            DataTypePointer type{};
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

        struct Literal;
        struct Name;
        struct Addition;
        struct Subtraction;
        struct Multiplication;
        struct Division;
        struct FunctionCall;

        struct ExpressionVisitor {
            virtual void visit(Literal& expression) = 0;
            virtual void visit(Name& expression) = 0;
            virtual void visit(Addition& expression) = 0;
            virtual void visit(Subtraction& expression) = 0;
            virtual void visit(Multiplication& expression) = 0;
            virtual void visit(Division& expression) = 0;
            virtual void visit(FunctionCall& expression) = 0;

            virtual ~ExpressionVisitor() = default;
        };

        struct Expression {
            virtual void accept(ExpressionVisitor& visitor) = 0;

            virtual ~Expression() = default;

            DataTypePointer data_type{};
        };

        template<typename T>
        struct ExpressionAcceptor : public Expression {
            void accept(ExpressionVisitor& visitor) final {
                visitor.visit(static_cast<T&>(*this));
            }
        };

        struct Literal : public ExpressionAcceptor<Literal> {
            explicit Literal(IntegerLiteral value) : value{ value } { }

            IntegerLiteral value;
        };

        struct Name : public ExpressionAcceptor<Name> {
            explicit Name(Lexer::Tokens::Identifier name) : name{ name } { }

            Lexer::Tokens::Identifier name;
        };

        struct BinaryOperator {
            BinaryOperator(std::unique_ptr<Expression> lhs, std::unique_ptr<Expression> rhs)
                : lhs{ std::move(lhs) },
                  rhs{ std::move(rhs) } { }

            std::unique_ptr<Expression> lhs;
            std::unique_ptr<Expression> rhs;
        };

        struct Addition : public BinaryOperator, public ExpressionAcceptor<Addition> {
            using BinaryOperator::BinaryOperator;
        };

        struct Subtraction : public BinaryOperator, public ExpressionAcceptor<Subtraction> {
            using BinaryOperator::BinaryOperator;
        };

        struct Multiplication : public BinaryOperator, public ExpressionAcceptor<Multiplication> {
            using BinaryOperator::BinaryOperator;
        };

        struct Division : public BinaryOperator, public ExpressionAcceptor<Division> {
            using BinaryOperator::BinaryOperator;
        };

        struct FunctionCall : public ExpressionAcceptor<FunctionCall> {
            FunctionCall(std::unique_ptr<Expression> callee, std::vector<std::unique_ptr<Expression>> arguments)
                : callee{ std::move(callee) },
                  arguments{ std::move(arguments) } { }

            std::unique_ptr<Expression> callee;
            std::vector<std::unique_ptr<Expression>> arguments;
        };

    }// namespace Expressions

    struct Parameter {
        Identifier name;
        DataTypePointer type{};
    };

    using ParameterList = std::vector<Parameter>;

    struct FunctionDefinition {
        Identifier name;
        ParameterList parameters;
        DataTypePointer return_type{};
        Statements::Block body;
    };

    template<typename... T>
    using PointerVariant = std::variant<std::unique_ptr<T>...>;

    using Program = std::vector<PointerVariant<FunctionDefinition>>;

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens);

}// namespace Parser
