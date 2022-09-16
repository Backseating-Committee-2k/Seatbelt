//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include "data_type.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "overloaded.hpp"
#include "parameter_list.hpp"
#include "scope.hpp"
#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

/**
 * This type only serves as a marker inside the `function_to_call`-member of
 * FunctionCall-expressions. If the variant has this type, it means that the
 * function call has a function pointer as target.
 */
struct FunctionPointerMarker { };

enum class ValueType {
    MutableLValue,
    ConstLValue,
    RValue,
    Undetermined,
};

namespace Parser {
    using namespace Lexer::Tokens;

    struct FunctionDefinition;

    namespace Expressions {
        struct Expression;
    }

    struct IndexOperator { };

    using BinaryOperatorType = std::variant<Token, IndexOperator>;

    [[nodiscard]] inline std::string_view binary_operator_type_to_string_view(const BinaryOperatorType& operator_type) {
        if (const auto operator_token = std::get_if<Token>(&operator_type)) {
            return Error::token_location(*operator_token).view();
        } else if (std::holds_alternative<IndexOperator>(operator_type)) {
            return "[]";
        } else {
            assert(false and "unreachable");
            return "";
        }
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
            Block(LeftCurlyBracket opening_bracket_token, StatementList statements)
                : opening_bracket_token{ opening_bracket_token },
                  statements{ std::move(statements) } { }

            LeftCurlyBracket opening_bracket_token;
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
            explicit BreakStatement(Lexer::Tokens::Break break_token) : break_token{ break_token } { }

            Lexer::Tokens::Break break_token;
        };

        struct ContinueStatement : public StatementAcceptor<ContinueStatement> {
            explicit ContinueStatement(Lexer::Tokens::Continue continue_token) : continue_token{ continue_token } { }

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
            VariableDefinition(
                    Let let_token,
                    Identifier name,
                    Equals equals_token,
                    std::span<const Token> type_definition_tokens,
                    std::unique_ptr<DataType> type_definition,
                    std::unique_ptr<Expression> initial_value,
                    Mutability binding_mutability
            )
                : let_token{ let_token },
                  name{ name },
                  equals_token{ equals_token },
                  type_definition_tokens{ type_definition_tokens },
                  type_definition{ std::move(type_definition) },
                  initial_value{ std::move(initial_value) },
                  binding_mutability{ binding_mutability } { }

            Let let_token;
            Identifier name;
            Equals equals_token;
            std::span<const Token> type_definition_tokens;
            std::unique_ptr<DataType> type_definition;
            const DataType* type{ nullptr };
            std::unique_ptr<Expression> initial_value;
            VariableSymbol* variable_symbol{ nullptr };
            Mutability binding_mutability;
        };

        struct InlineAssembly : public StatementAcceptor<InlineAssembly> {
            explicit InlineAssembly(Lexer::Tokens::InlineAssembly token) : token{ token } { }

            Lexer::Tokens::InlineAssembly token;
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
    } // namespace Statements

    namespace Expressions {

        struct Integer;
        struct Char;
        struct Bool;
        struct ArrayLiteral;
        struct Name;
        struct UnaryOperator;
        struct BinaryOperator;
        struct FunctionCall;
        struct Assignment;
        struct Nothing;
        struct TypeSizeExpression;
        struct ValueSizeExpression;

        struct ExpressionVisitor {
            virtual void visit(Integer& expression) = 0;
            virtual void visit(Char& expression) = 0;
            virtual void visit(Bool& expression) = 0;
            virtual void visit(Name& expression) = 0;
            virtual void visit(ArrayLiteral& expression) = 0;
            virtual void visit(UnaryOperator& expression) = 0;
            virtual void visit(BinaryOperator& expression) = 0;
            virtual void visit(FunctionCall& expression) = 0;
            virtual void visit(Assignment& expression) = 0;
            virtual void visit(Nothing& expression) = 0;
            virtual void visit(TypeSizeExpression& expression) = 0;
            virtual void visit(ValueSizeExpression& expression) = 0;

            virtual ~ExpressionVisitor() = default;
        };

        struct Expression {
            virtual ~Expression() = default;
            virtual void accept(ExpressionVisitor& visitor) = 0;

            const DataType* data_type{ nullptr };
            const Scope* surrounding_scope{ nullptr };
            ValueType value_type{ ValueType::Undetermined };

            [[nodiscard]] bool is_lvalue() const {
                return value_type == ValueType::ConstLValue or value_type == ValueType::MutableLValue;
            }
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
            std::string emittable_string{};
        };

        struct Char : public ExpressionAcceptor<Char> {
            explicit Char(CharLiteral value) : value{ value } { }

            CharLiteral value;
        };

        struct Bool : public ExpressionAcceptor<Bool> {
            explicit Bool(BoolLiteral value) : value{ value } { }

            BoolLiteral value;
        };

        struct ArrayLiteral : public ExpressionAcceptor<ArrayLiteral> {
            ArrayLiteral(
                    LeftSquareBracket left_square_bracket_token,
                    std::variant<
                            std::vector<std::unique_ptr<Expression>>,
                            std::pair<std::unique_ptr<Expression>, usize>> values
            )
                : left_square_bracket_token{ left_square_bracket_token },
                  values{ std::move(values) } { }

            LeftSquareBracket left_square_bracket_token;
            std::variant<std::vector<std::unique_ptr<Expression>>, std::pair<std::unique_ptr<Expression>, usize>>
                    values;
        };

        struct Name : public ExpressionAcceptor<Name> {
            explicit Name(std::span<const Lexer::Tokens::Token> name_tokens) : name_tokens{ name_tokens } { }

            std::span<const Lexer::Tokens::Token> name_tokens;
            std::optional<std::vector<const FunctionOverload*>> possible_overloads{};
            std::optional<std::vector<const TypeOverload*>> possible_type_overloads{};
            std::optional<const VariableSymbol*> variable_symbol{};
        };

        struct UnaryOperator : public ExpressionAcceptor<UnaryOperator> {
            UnaryOperator(Token operator_token, std::unique_ptr<Expression> operand)
                : operator_token{ operator_token },
                  operand{ std::move(operand) } { }

            Token operator_token;
            std::unique_ptr<Expression> operand;
        };

        struct BinaryOperator : public ExpressionAcceptor<BinaryOperator> {
            BinaryOperator(
                    std::unique_ptr<Expression> lhs,
                    std::unique_ptr<Expression> rhs,
                    BinaryOperatorType operator_type
            )
                : lhs{ std::move(lhs) },
                  rhs{ std::move(rhs) },
                  operator_type{ operator_type } { }

            std::unique_ptr<Expression> lhs;
            std::unique_ptr<Expression> rhs;
            BinaryOperatorType operator_type;
        };

        struct FunctionCall : public ExpressionAcceptor<FunctionCall> {
            FunctionCall(
                    std::unique_ptr<Expression> callee,
                    LeftParenthesis left_parenthesis,
                    std::vector<std::unique_ptr<Expression>> arguments
            )
                : callee{ std::move(callee) },
                  left_parenthesis{ left_parenthesis },
                  arguments{ std::move(arguments) } { }

            std::unique_ptr<Expression> callee;
            LeftParenthesis left_parenthesis;
            std::vector<std::unique_ptr<Expression>> arguments;
            std::variant<std::monostate, const FunctionDefinition*, FunctionPointerMarker> function_to_call{};
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

        struct Nothing : public ExpressionAcceptor<Nothing> {
            explicit Nothing(NothingLiteral nothing_token) : nothing_token{ nothing_token } { }

            NothingLiteral nothing_token;
        };

        struct TypeSizeExpression : public ExpressionAcceptor<TypeSizeExpression> {
            TypeSizeExpression(
                    TypeSize type_size_token,
                    std::unique_ptr<DataType> type_definition,
                    std::span<const Token> contained_data_type_tokens
            )
                : type_size_token{ type_size_token },
                  type_definition{ std::move(type_definition) },
                  contained_data_type_tokens{ contained_data_type_tokens } { }

            TypeSize type_size_token;
            std::unique_ptr<DataType> type_definition;
            const DataType* contained_data_type{ nullptr };
            std::span<const Token> contained_data_type_tokens;
        };

        struct ValueSizeExpression : public ExpressionAcceptor<ValueSizeExpression> {
            explicit ValueSizeExpression(ValueSize value_size_token, std::unique_ptr<Expression> expression)
                : value_size_token{ value_size_token },
                  expression{ std::move(expression) } { }

            ValueSize value_size_token;
            std::unique_ptr<Expression> expression;
        };


    } // namespace Expressions

    struct FunctionDefinition;
    struct ImportStatement;
    struct CustomTypeDefinition;
    struct NamespaceDefinition;

    template<typename... T>
    using PointerVariant = std::variant<std::unique_ptr<T>...>;

    using Program = std::vector<PointerVariant<FunctionDefinition, ImportStatement, CustomTypeDefinition, NamespaceDefinition>>;

    struct ImportStatement {
        Import import_token;
        std::span<const Token> import_path_tokens;
    };

    struct NamespaceDefinition {
        Namespace namespace_token;
        Identifier name;
        Program contents;
        std::unique_ptr<Scope> scope;
    };

    struct VariantMemberDefinition {
        Identifier name;
        std::unique_ptr<DataType> type_definition;
        std::span<const Token> type_definition_tokens;
        const DataType* type{ nullptr };
    };

    struct VariantDefinition {
        Identifier name;
        std::vector<VariantMemberDefinition> members;
    };

    struct CustomTypeDefinition {
        std::optional<Export> export_token;
        Type type_token;
        std::optional<Identifier> name;
        std::optional<Restricted> restricted_token;
        LeftCurlyBracket left_curly_bracket;
        std::map<u32, VariantDefinition> alternatives;
        RightCurlyBracket right_curly_bracket;
        std::string namespace_name{};

        [[nodiscard]] bool is_restricted() const {
            return restricted_token.has_value();
        }

        [[nodiscard]] bool is_exported() const {
            return export_token.has_value();
        }
    };

    struct FunctionDefinition {
        Identifier name;
        ParameterList parameters;
        std::unique_ptr<DataType> return_type_definition;
        std::span<const Token> return_type_definition_tokens{};
        std::optional<Export> export_token{};
        const DataType* return_type{ nullptr };
        Statements::Block body;
        FunctionOverload* corresponding_symbol{ nullptr };
        bool is_entry_point{ false };
        std::vector<Statements::LabelDefinition*> contained_labels{};
        std::optional<usize> occupied_stack_space{};   // total size of the needed stack space (in bytes)
        std::optional<usize> parameters_stack_space{}; // size of all parameters (in bytes)
        const Scope* surrounding_scope{ nullptr };

        [[nodiscard]] bool is_exported() const {
            return export_token.has_value();
        }
    };

    void concatenate_programs(Program& first, Program&& second);

    using NamespacesMap = std::unordered_map<std::string, NamespaceDefinition*>;

    [[nodiscard]] std::pair<Program, NamespacesMap>
    parse(const Lexer::TokenList& tokens, TypeContainer& type_container, NamespacesMap previous_namespaces);

} // namespace Parser
