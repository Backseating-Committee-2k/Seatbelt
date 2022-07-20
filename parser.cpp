//
// Created by coder2k on 17.06.2022.
//

#include "parser.hpp"
#include "error.hpp"
#include "namespace.hpp"
#include "types.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <memory>
#include <optional>
#include <ranges>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#define DEFINE_BINARY_OPERATOR_PARSER_FUNCTION(name, token, next)                          \
    [[nodiscard]] std::unique_ptr<Expression> name() {                                     \
        using Expressions::BinaryOperator;                                                 \
        auto accumulator = next();                                                         \
        while (auto current_token = maybe_consume<token>()) {                              \
            auto next_operand = next();                                                    \
            accumulator = std::make_unique<BinaryOperator>(                                \
                    std::move(accumulator), std::move(next_operand), current_token.value() \
            );                                                                             \
        }                                                                                  \
        return accumulator;                                                                \
    }

#define DEFINE_BINARY_OPERATOR_PARSER_FUNCTION_2(name, first_token, second_token, next)    \
    [[nodiscard]] std::unique_ptr<Expression> name() {                                     \
        using Expressions::BinaryOperator;                                                 \
        auto accumulator = next();                                                         \
        while (auto current_token = maybe_consume_one_of<first_token, second_token>()) {   \
            auto next_operand = next();                                                    \
            accumulator = std::make_unique<BinaryOperator>(                                \
                    std::move(accumulator), std::move(next_operand), current_token.value() \
            );                                                                             \
        }                                                                                  \
        return accumulator;                                                                \
    }

namespace Parser {

    using Expressions::Expression;

    class ParserState {
    public:
        explicit ParserState(const Lexer::TokenList& tokens) : m_tokens{ &tokens } { }

        [[nodiscard]] Program parse() {
            auto program = parse_header();
            concatenate_programs(program, parse_body());
            return program;
        }

        [[nodiscard]] Program parse_header() {
            auto program = Program{};
            while (not end_of_file() and current_is<Import>()) {
                program.push_back(import_statement());
            }
            return program;
        }

        [[nodiscard]] Program parse_body() {
            auto program = Program{};
            while (not end_of_file()) {
                if (current_is<Namespace>()) {
                    concatenate_programs(program, parse_namespace());
                } else if (current_is<Function>()) {
                    program.push_back(function_definition());
                } else if (current_is<Import>()) {
                    Error::error(current(), "imports must precede all other top level statements of a source file");
                } else {
                    break;
                }
            }
            return program;
        }

    private:
        [[nodiscard]] Program parse_namespace() {
            assert(current_is<Namespace>());
            advance();
            usize count = 1;
            m_namespaces_stack.emplace_back(consume<Identifier>("expected identifier").location.view());
            while (maybe_consume<DoubleColon>()) {
                m_namespaces_stack.emplace_back(consume<Identifier>("expected identifier").location.view());
                ++count;
            }
            consume<LeftCurlyBracket>("expected \"{\"");
            auto namespace_contents = parse_body();
            m_namespaces_stack.resize(m_namespaces_stack.size() - count);
            consume<RightCurlyBracket>("expected \"}\"");
            return namespace_contents;
        }

        [[nodiscard]] std::unique_ptr<Expression> expression() {
            return logical_or();
        }

        DEFINE_BINARY_OPERATOR_PARSER_FUNCTION(logical_or, Or, logical_and)

        DEFINE_BINARY_OPERATOR_PARSER_FUNCTION(logical_and, And, addition_or_subtraction)

        DEFINE_BINARY_OPERATOR_PARSER_FUNCTION_2(addition_or_subtraction, Plus, Minus, multiplication_or_division)

        DEFINE_BINARY_OPERATOR_PARSER_FUNCTION_2(multiplication_or_division, Asterisk, ForwardSlash, function_call)

        [[nodiscard]] std::unique_ptr<Expression> function_call() {
            using Expressions::FunctionCall;

            auto accumulator = this->primary();
            while (maybe_consume<LeftParenthesis>()) {
                std::vector<std::unique_ptr<Expression>> arguments;
                while (not end_of_file() and not current_is<RightParenthesis>()) {
                    arguments.push_back(expression());
                    if (not maybe_consume<Comma>()) {
                        break;
                    }
                }
                consume<RightParenthesis>("expected \")\" at end of parameter list");
                accumulator = std::make_unique<FunctionCall>(std::move(accumulator), std::move(arguments));
            }
            return accumulator;
        }

        [[nodiscard]] std::unique_ptr<Expression> primary() {
            using namespace Expressions;

            if (maybe_consume<LeftParenthesis>()) {
                auto sub_expression = expression();
                consume<RightParenthesis>("expected \")\"");
                return sub_expression;
            }
            if (const auto integer_literal_token = maybe_consume<IntegerLiteral>()) {
                return std::make_unique<Integer>(integer_literal_token.value());
            }
            if (const auto char_literal_token = maybe_consume<CharLiteral>()) {
                return std::make_unique<Char>(char_literal_token.value());
            }
            if (const auto bool_literal_token = maybe_consume<BoolLiteral>()) {
                return std::make_unique<Bool>(bool_literal_token.value());
            }
            if (current_is<Identifier>()) {
                const usize name_start = m_index;
                advance();
                while (maybe_consume<DoubleColon>()) {
                    consume<Identifier>("expected identifier");
                }
                const usize name_end = m_index;
                return std::make_unique<Name>(std::span{ std::cbegin(*m_tokens) + name_start,
                                                         std::cbegin(*m_tokens) + name_end });
            }
            error("unexpected token");
            return nullptr;
        }

        [[nodiscard]] std::unique_ptr<FunctionDefinition> function_definition() {
            assert(current_is<Function>());
            advance();
            const auto name = consume<Identifier>("expected function name");
            consume<LeftParenthesis>("expected \"(\"");
            auto parameters = ParameterList{};
            while (not end_of_file() and not current_is<RightParenthesis>()) {
                const auto parameter_name = consume<Identifier>("expected parameter name");
                consume<Colon>("expected \":\"");
                const auto type_tokens = type();
                parameters.push_back(Parameter{ .name{ parameter_name }, .type_tokens{ type_tokens }, .type{} });
                if (not maybe_consume<Comma>()) {
                    break;
                }
            }
            consume<RightParenthesis>("expected \")\"");
            consume<Colon>("expected \":\"");
            const auto return_type_tokens = type();
            auto body = block();
            auto namespace_name = get_namespace_qualifier(m_namespaces_stack);
            return std::make_unique<FunctionDefinition>(FunctionDefinition{
                    .name{ name },
                    .parameters{ std::move(parameters) },
                    .return_type_tokens{ return_type_tokens },
                    .return_type{},
                    .body{ std::move(body) },
                    .namespace_name{ std::move(namespace_name) } });
        }

        [[nodiscard]] std::unique_ptr<ImportStatement> import_statement() {
            assert(current_is<Import>());
            const auto import_token = consume<Import>("error should be impossible here");
            const usize path_start = m_index;
            consume<Identifier>("expected identifier");
            while (maybe_consume<Dot>()) {
                consume<Identifier>("expected identifier");
            }
            const usize path_end = m_index;
            consume<Semicolon>("expected \";\"");
            return std::make_unique<ImportStatement>(ImportStatement{
                    .import_token{ import_token },
                    .import_path_tokens{ std::span{
                            std::begin(*m_tokens) + static_cast<Lexer::TokenList::difference_type>(path_start),
                            std::begin(*m_tokens) + static_cast<Lexer::TokenList::difference_type>(path_end),
                    } },
            });
        }

        [[nodiscard]] std::unique_ptr<Statements::IfStatement> if_statement() {
            const auto if_token = consume<If>();
            auto condition = expression();
            auto then_block = block();
            auto else_block = Statements::Block{ Statements::StatementList{} };
            auto else_token = std::optional<Else>{};
            if ((else_token = maybe_consume<Else>())) {
                if (current_is<If>()) {
                    else_block.statements.push_back(if_statement());
                } else {
                    else_block = block();
                }
            }
            return std::make_unique<Statements::IfStatement>(
                    if_token, std::move(condition), std::move(then_block), else_token, std::move(else_block)
            );
        }

        [[nodiscard]] std::unique_ptr<Statements::LoopStatement> loop_statement() {
            const auto loop_token = consume<Loop>();
            return std::make_unique<Parser::Statements::LoopStatement>(loop_token, block());
        }

        [[nodiscard]] std::unique_ptr<Statements::BreakStatement> break_statement() {
            const auto loop_token = consume<Break>();
            consume<Semicolon>("expected \";\"");
            return std::make_unique<Parser::Statements::BreakStatement>(loop_token);
        }

        [[nodiscard]] std::unique_ptr<Statements::ContinueStatement> continue_statement() {
            const auto continue_token = consume<Continue>();
            consume<Semicolon>("expected \";\"");
            return std::make_unique<Parser::Statements::ContinueStatement>(continue_token);
        }

        [[nodiscard]] std::unique_ptr<Statements::WhileStatement> while_statement() {
            const auto while_token = consume<While>();
            auto condition = expression();
            auto body = block();
            return std::make_unique<Statements::WhileStatement>(while_token, std::move(condition), std::move(body));
        }

        [[nodiscard]] std::unique_ptr<Statements::DoWhileStatement> do_while_statement() {
            const auto do_token = consume<Do>();
            auto body = block();
            const auto while_token = consume<While>("expected \"while\"");
            auto condition = expression();
            consume<Semicolon>("expected \";\"");
            return std::make_unique<Statements::DoWhileStatement>(
                    do_token, std::move(body), while_token, std::move(condition)
            );
        }

        [[nodiscard]] std::unique_ptr<Statements::ForStatement> for_statement() {
            const auto for_token = consume<For>();
            const auto uses_parentheses = maybe_consume<LeftParenthesis>();
            auto initializer = std::unique_ptr<Statements::Statement>{};
            if (current_is<Let>()) {
                initializer = variable_definition();
            } else if (not current_is<Semicolon>()) {
                initializer = std::make_unique<Statements::ExpressionStatement>(expression());
                consume<Semicolon>("expected \";\"");
            } else {
                consume<Semicolon>();
            }

            auto condition = std::unique_ptr<Expression>{};
            if (not current_is<Semicolon>()) {
                condition = expression();
            }
            consume<Semicolon>("expected \";\"");

            auto increment = std::unique_ptr<Expression>{};
            if (not current_is<RightParenthesis>()) {
                increment = expression();
            }

            if (uses_parentheses) {
                consume<RightParenthesis>("expected \")\"");
            }

            auto body = block();
            return std::make_unique<Statements::ForStatement>(
                    for_token, std::move(initializer), std::move(condition), std::move(increment), std::move(body)
            );
        }

        [[nodiscard]] Statements::Block block() {
            Statements::StatementList statements;
            consume<LeftCurlyBracket>("expected \"{\"");
            while (not end_of_file() and not current_is<RightCurlyBracket>()) {
                if (current_is<If>()) {
                    statements.push_back(if_statement());
                } else if (current_is<Loop>()) {
                    statements.push_back(loop_statement());
                } else if (current_is<Break>()) {
                    statements.push_back(break_statement());
                } else if (current_is<Continue>()) {
                    statements.push_back(continue_statement());
                } else if (current_is<While>()) {
                    statements.push_back(while_statement());
                } else if (current_is<Do>()) {
                    statements.push_back(do_while_statement());
                } else if (current_is<For>()) {
                    statements.push_back(for_statement());
                } else if (current_is<Let>()) {
                    statements.push_back(variable_definition());
                } else if (current_is<LeftCurlyBracket>()) {
                    statements.push_back(std::make_unique<Statements::Block>(block()));
                } else if (current_is<InlineAssembly>()) {
                    statements.push_back(
                            std::make_unique<Statements::InlineAssembly>(&std::get<InlineAssembly>(current()))
                    );
                    advance();
                } else {
                    auto expression = this->expression();
                    consume<Semicolon>("expected \";\" to complete expression statement");
                    statements.push_back(std::make_unique<Statements::ExpressionStatement>(std::move(expression)));
                }
            }
            consume<RightCurlyBracket>("expected \"}\"");
            return Statements::Block{ std::move(statements) };
        }

        [[nodiscard]] std::span<const Token> type() {
            const auto type_start_index = m_index;
            while (current_is<Arrow>()) {
                advance();
            }
            consume<Identifier>("typanem expected");
            return std::span{ std::begin(*m_tokens) + static_cast<ptrdiff_t>(type_start_index),
                              std::begin(*m_tokens) + static_cast<ptrdiff_t>(m_index) };
        }

        std::unique_ptr<Statements::VariableDefinition> variable_definition() {
            assert(current_is<Let>());
            advance();
            const auto identifier = consume<Identifier>("expected variable name");
            consume<Colon>("expected \":\"");
            auto type_tokens = type();
            auto equals_token = consume<Equals>("expected variable initialization");
            auto initial_value = expression();
            consume<Semicolon>("expected \";\"");
            return std::make_unique<Statements::VariableDefinition>(
                    identifier, equals_token, type_tokens, std::move(initial_value)
            );
        }

        template<typename T>
        [[nodiscard]] bool current_is() const {
            return std::holds_alternative<T>(current());
        }

        template<typename T>
        [[nodiscard]] bool next_is() const {
            return std::holds_alternative<T>(peek());
        }

        template<typename T>
        T consume(const std::string_view message = "") {
            if (not current_is<T>()) {
                if (message.empty()) {
                    assert(false and "this error should be unreachable");
                }
                error(message);
            }
            const auto result = std::get<T>(current());
            advance();
            return result;
        }

        template<typename T>
        std::optional<T> maybe_consume() {
            if (current_is<T>()) {
                const auto result = std::get<T>(current());
                advance();
                return result;
            }
            return {};
        }

        template<typename FirstType, typename... RemainingTypes>
        std::optional<Token> maybe_consume_one_of() {
            if constexpr (sizeof...(RemainingTypes) == 0) {
                return maybe_consume<FirstType>();
            } else {
                auto result = maybe_consume<FirstType>();
                if (result) {
                    return result;
                }
                return maybe_consume_one_of<RemainingTypes...>();
            }
        }

        [[nodiscard]] const Token& current() const {
            return (*m_tokens)[m_index];
        }

        [[nodiscard]] const Token& peek() const {
            return (*m_tokens)[m_index + 1];
        }

        [[nodiscard]] bool end_of_file() const {
            return m_index >= m_tokens->size() || current_is<EndOfFile>();
        }

        void advance() {
            ++m_index;
        }

        void error(const std::string_view message) const {
            Error::error(current(), message);
        }

    private:
        usize m_index{ 0 };
        const Lexer::TokenList* m_tokens;
        NamespacesStack m_namespaces_stack{};
    };

    void concatenate_programs(Program& first, Program&& second) {
        first.reserve(first.size() + second.size());
        for (auto& top_level_statement : second) {
            first.push_back(std::move(top_level_statement));
        }
    }

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens) {
        auto parser_state = ParserState{ tokens };
        return parser_state.parse();
    }

}// namespace Parser
