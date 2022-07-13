//
// Created by coder2k on 17.06.2022.
//

#include <variant>
#include <optional>
#include <algorithm>
#include <ranges>
#include <utility>
#include <cassert>
#include <memory>
#include <string_view>
#include <fmt/core.h>
#include "parser.hpp"
#include "error.hpp"
#include "types.hpp"

namespace Parser {

    using Expressions::Expression;

    class ParserState {
    public:
        explicit ParserState(const Lexer::TokenList& tokens) : m_tokens{ &tokens } { }

        [[nodiscard]] Program parse() {
            auto program = Program{};
            while (not end_of_file()) {
                if (current_is<Function>()) {
                    program.push_back(function_definition());
                } else {
                    error("\"function\" keyword expected");
                }
            }
            return program;
        }

    private:
        [[nodiscard]] std::unique_ptr<Expression> expression() {
            return addition_or_subtraction();
        }

        [[nodiscard]] std::unique_ptr<Expression> addition_or_subtraction() {
            using Expressions::BinaryOperator;
            auto accumulator = multiplication_or_division();
            while (auto token = maybe_consume_one_of<Plus, Minus>()) {
                auto next_operand = multiplication_or_division();
                accumulator = std::make_unique<BinaryOperator>(
                        std::move(accumulator), std::move(next_operand), token.value()
                );
            }
            return accumulator;
        }

        [[nodiscard]] std::unique_ptr<Expression> multiplication_or_division() {
            using Expressions::BinaryOperator;
            auto accumulator = function_call();
            while (auto token = maybe_consume_one_of<Asterisk, ForwardSlash>()) {
                auto next_operand = function_call();
                accumulator = std::make_unique<BinaryOperator>(
                        std::move(accumulator), std::move(next_operand), token.value()
                );
            }
            return accumulator;
        }

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
            } else if (auto literal_token = maybe_consume<IntegerLiteral>()) {
                return std::make_unique<Literal>(literal_token.value());
            }
            if (auto identifier_token = maybe_consume<Identifier>()) {
                return std::make_unique<Name>(identifier_token.value());
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
            return std::make_unique<FunctionDefinition>(FunctionDefinition{ .name{ name },
                                                                            .parameters{ std::move(parameters) },
                                                                            .return_type_tokens{ return_type_tokens },
                                                                            .return_type{},
                                                                            .body{ std::move(body) } });
        }

        [[nodiscard]] Statements::Block block() {
            Statements::StatementList statements;
            consume<LeftCurlyBracket>("expected \"{\"");
            while (not end_of_file() and not current_is<RightCurlyBracket>()) {
                if (current_is<Let>()) {
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
                    /*Error::error(
                            current(), fmt::format(
                                               "unexpected token: \"{}\"",
                                               std::visit([](auto&& token) { return token.debug_name; }, current())
                                       )
                    );
                    advance();*/
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
        T consume(const std::string_view message) {
            if (not current_is<T>()) {
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
    };

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens) {
        auto parser_state = ParserState{ tokens };
        return parser_state.parse();
    }

}// namespace Parser
