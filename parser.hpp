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

// explicit deduction guide (not needed as of C++20)
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

namespace Parser {
    using namespace Lexer::Tokens;

    struct FunctionDefinition;

    namespace Expressions {
        struct Expression;
    }

    namespace Statements {

        using Expressions::Expression;

        struct Block;
        struct IfStatement;
        struct LoopStatement;
        struct BreakStatement;
        struct ContinueStatement;
        struct WhileStatement;
        struct DoWhileStatement;
        struct ForStatement;
        struct ReturnStatement;
        struct VariableDefinition;
        struct InlineAssembly;
        struct ExpressionStatement;
        struct LabelDefinition;
        struct GotoStatement;

        struct StatementVisitor {
            virtual void visit(Block& statement) = 0;
            virtual void visit(IfStatement& statement) = 0;
            virtual void visit(LoopStatement& statement) = 0;
            virtual void visit(BreakStatement& statement) = 0;
            virtual void visit(ContinueStatement& statement) = 0;
            virtual void visit(WhileStatement& statement) = 0;
            virtual void visit(DoWhileStatement& statement) = 0;
            virtual void visit(ForStatement& statement) = 0;
            virtual void visit(ReturnStatement& statement) = 0;
            virtual void visit(VariableDefinition& statement) = 0;
            virtual void visit(InlineAssembly& statement) = 0;
            virtual void visit(ExpressionStatement& statement) = 0;
            virtual void visit(LabelDefinition& statement) = 0;
            virtual void visit(GotoStatement& statement) = 0;

            virtual ~StatementVisitor() = default;
        };

        struct Statement {
            virtual ~Statement() = default;

            virtual void accept(StatementVisitor& visitor) = 0;

            const Scope* surrounding_scope{ nullptr };
        };

        template<typename T>
        struct StatementAcceptor : public Statement {
            void accept(StatementVisitor& visitor) final {
                visitor.visit(static_cast<T&>(*this));
            }
        };

        using StatementList = std::vector<std::unique_ptr<Statement>>;

        struct Block : public StatementAcceptor<Block> {
            explicit Block(StatementList statements) : statements{ std::move(statements) } { }

            StatementList statements;
            std::unique_ptr<Scope> scope;
            std::optional<usize> occupied_stack_space{};
        };

        struct IfStatement : public StatementAcceptor<IfStatement> {
            IfStatement(
                    Lexer::Tokens::If if_token,
                    std::unique_ptr<Expression> condition,
                    Block then_block,
                    std::optional<Lexer::Tokens::Else> else_token,
                    Block else_block
            )
                : if_token{ if_token },
                  condition{ std::move(condition) },
                  then_block{ std::move(then_block) },
                  else_token{ else_token },
                  else_block{ std::move(else_block) } { }

            Lexer::Tokens::If if_token;
            std::unique_ptr<Expression> condition;
            Block then_block;
            std::optional<Lexer::Tokens::Else> else_token;
            Block else_block;
        };

        struct LoopStatement : public StatementAcceptor<LoopStatement> {
            LoopStatement(Lexer::Tokens::Loop loop_token, Block body)
                : loop_token{ loop_token },
                  body{ std::move(body) } { }

            Lexer::Tokens::Loop loop_token;
            Block body;
        };

        struct BreakStatement : public StatementAcceptor<BreakStatement> {
            BreakStatement(Lexer::Tokens::Break break_token) : break_token{ break_token } { }

            Lexer::Tokens::Break break_token;
        };

        struct ContinueStatement : public StatementAcceptor<ContinueStatement> {
            ContinueStatement(Lexer::Tokens::Continue continue_token) : continue_token{ continue_token } { }

            Lexer::Tokens::Continue continue_token;
        };

        struct WhileStatement : public StatementAcceptor<WhileStatement> {
            WhileStatement(While while_token, std::unique_ptr<Expression> condition, Block body)
                : while_token{ while_token },
                  condition{ std::move(condition) },
                  body{ std::move(body) } { }

            While while_token;
            std::unique_ptr<Expression> condition;
            Block body;
        };

        struct DoWhileStatement : public StatementAcceptor<DoWhileStatement> {
            DoWhileStatement(Do do_token, Block body, While while_token, std::unique_ptr<Expression> condition)
                : do_token{ do_token },
                  body{ std::move(body) },
                  while_token{ while_token },
                  condition{ std::move(condition) } { }

            Do do_token;
            Block body;
            While while_token;
            std::unique_ptr<Expression> condition;
        };

        struct ForStatement : public StatementAcceptor<ForStatement> {
            ForStatement(
                    For for_token,
                    std::unique_ptr<Statement> initializer,
                    std::unique_ptr<Expression> condition,
                    std::unique_ptr<Expression> increment,
                    Block body
            )
                : for_token{ for_token },
                  initializer{ std::move(initializer) },
                  condition{ std::move(condition) },
                  increment{ std::move(increment) },
                  body{ std::move(body) } { }

            For for_token;
            std::unique_ptr<Statement> initializer;
            std::unique_ptr<Expression> condition;
            std::unique_ptr<Expression> increment;
            Block body;
        };

        struct ReturnStatement : public StatementAcceptor<ReturnStatement> {
            ReturnStatement(Return return_token, std::unique_ptr<Expression> return_value)
                : return_token{ return_token },
                  return_value{ std::move(return_value) } { }

            Return return_token;
            std::unique_ptr<Expression> return_value;
        };

        struct VariableDefinition : public StatementAcceptor<VariableDefinition> {
            explicit VariableDefinition(
                    Let let_token,
                    Identifier name,
                    Equals equals_token,
                    std::unique_ptr<DataType> type_definition,
                    std::unique_ptr<Expression> initial_value
            )
                : let_token{ let_token },
                  name{ name },
                  equals_token{ equals_token },
                  type_definition{ std::move(type_definition) },
                  initial_value{ std::move(initial_value) } { }

            Let let_token;
            Identifier name;
            Equals equals_token;
            std::unique_ptr<DataType> type_definition;
            const DataType* type{ nullptr };
            std::unique_ptr<Expression> initial_value;
            VariableSymbol* variable_symbol{ nullptr };
        };

        struct InlineAssembly : public StatementAcceptor<InlineAssembly> {
            explicit InlineAssembly(const Lexer::Tokens::InlineAssembly* token) : token{ token } { }

            const Lexer::Tokens::InlineAssembly* token;
        };

        struct ExpressionStatement : public StatementAcceptor<ExpressionStatement> {
            explicit ExpressionStatement(std::unique_ptr<Expression> expression)
                : expression{ std::move(expression) } { }

            std::unique_ptr<Expression> expression;
        };

        struct LabelDefinition : public StatementAcceptor<LabelDefinition> {
            LabelDefinition(Label label_token, Identifier identifier)
                : label_token{ label_token },
                  identifier{ identifier } { }

            Label label_token;
            Identifier identifier;
            std::string emitted_label{};
        };

        struct GotoStatement : public StatementAcceptor<GotoStatement> {
            GotoStatement(Goto goto_token, Identifier label_identifier)
                : goto_token{ goto_token },
                  label_identifier{ label_identifier } { }

            Goto goto_token;
            Identifier label_identifier;
            const FunctionDefinition* surrounding_function{ nullptr };
            const LabelDefinition* target_label{ nullptr };
        };
    }// namespace Statements

    namespace Expressions {

        struct Integer;
        struct Char;
        struct Bool;
        struct Name;
        struct BinaryOperator;
        struct FunctionCall;
        struct Assignment;

        struct ExpressionVisitor {
            virtual void visit(Integer& expression) = 0;
            virtual void visit(Char& expression) = 0;
            virtual void visit(Bool& expression) = 0;
            virtual void visit(Name& expression) = 0;
            virtual void visit(BinaryOperator& expression) = 0;
            virtual void visit(FunctionCall& expression) = 0;
            virtual void visit(Assignment& expression) = 0;

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

        struct Bool : public ExpressionAcceptor<Bool> {
            explicit Bool(BoolLiteral value) : value{ value } { }

            BoolLiteral value;
        };

        struct Name : public ExpressionAcceptor<Name> {
            explicit Name(std::span<const Lexer::Tokens::Token> name_tokens) : name_tokens{ name_tokens } { }

            std::span<const Lexer::Tokens::Token> name_tokens;
            std::optional<std::vector<const FunctionOverload*>> possible_overloads{};
            std::optional<const VariableSymbol*> variable_symbol{};
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
            const FunctionDefinition* function_to_call{ nullptr };
        };

        struct Assignment : public ExpressionAcceptor<Assignment> {
            Assignment(std::unique_ptr<Expression> assignee, Equals equals_token, std::unique_ptr<Expression> value)
                : assignee{ std::move(assignee) },
                  equals_token{ equals_token },
                  value{ std::move(value) } { }

            std::unique_ptr<Expression> assignee;
            Equals equals_token;
            std::unique_ptr<Expression> value;
        };

    }// namespace Expressions

    struct FunctionDefinition {
        Identifier name;
        ParameterList parameters;
        std::unique_ptr<DataType> return_type_definition;
        const DataType* return_type{ nullptr };
        Statements::Block body;
        FunctionOverload* corresponding_symbol{ nullptr };
        std::string namespace_name{};
        bool is_entry_point{ false };
        std::vector<Statements::LabelDefinition*> contained_labels{};
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
