//
// Created by coder2k on 17.06.2022.
//

#include <variant>
#include <optional>
#include <algorithm>
#include <ranges>
#include <iostream>
#include <format>
#include <utility>
#include <cassert>
#include <memory>
#include <string_view>
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
                    program.push_back(function());
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
            using Expressions::Addition, Expressions::Subtraction;
            auto accumulator = multiplication_or_division();

            bool did_consume;
            do {
                did_consume = false;
                while (maybe_consume<Plus>()) {
                    auto next_operand = multiplication_or_division();
                    accumulator = std::make_unique<Addition>(std::move(accumulator), std::move(next_operand));
                    did_consume = true;
                }
                while (maybe_consume<Minus>()) {
                    auto next_operand = multiplication_or_division();
                    accumulator = std::make_unique<Subtraction>(std::move(accumulator), std::move(next_operand));
                    did_consume = true;
                }
            } while (did_consume);
            return accumulator;
        }

        [[nodiscard]] std::unique_ptr<Expression> multiplication_or_division() {
            using Expressions::Multiplication, Expressions::Division;
            auto accumulator = primary();

            bool did_consume;
            do {
                did_consume = false;
                while (maybe_consume<Asterisk>()) {
                    auto next_operand = primary();
                    accumulator = std::make_unique<Multiplication>(std::move(accumulator), std::move(next_operand));
                    did_consume = true;
                }
                while (maybe_consume<ForwardSlash>()) {
                    auto next_operand = primary();
                    accumulator = std::make_unique<Division>(std::move(accumulator), std::move(next_operand));
                    did_consume = true;
                }
            } while (did_consume);
            return accumulator;
        }

        [[nodiscard]] std::unique_ptr<Expression> primary() {
            using namespace Expressions;
            if (maybe_consume<LeftParenthesis>()) {
                auto sub_expression = expression();
                consume<RightParenthesis>("expected \")\"");
                return sub_expression;
            } else if (auto literal_expression = maybe_consume<IntegerLiteral>()) {
                return std::make_unique<Literal>(literal_expression.value());
            } else if (auto name_expression = maybe_consume<Identifier>()) {
                return std::make_unique<Name>(name_expression.value());
            }
            error("unexpected token");
            return nullptr;
        }

        [[nodiscard]] std::unique_ptr<FunctionDefinition> function() {
            assert(current_is<Function>());
            advance();
            const auto name = consume<Identifier>("expected function name");
            consume<LeftParenthesis>("expected \"(\"");
            auto parameters = ParameterList{};
            while (not end_of_file() and not current_is<RightParenthesis>()) {
                const auto parameter_name = consume<Identifier>("expected parameter name");
                consume<Colon>("expected \":\"");
                const auto parameter_type = type();
                const auto parameter = Parameter{ .name{ parameter_name }, .type{ parameter_type } };
                parameters.push_back(parameter);
                if (not maybe_consume<Comma>()) {
                    break;
                }
            }
            consume<RightParenthesis>("expected \")\"");
            consume<Colon>("expected \":\"");
            const auto return_type = type();
            auto body = block();
            auto function_definition =
                    std::make_unique<FunctionDefinition>(FunctionDefinition{ .name{ name },
                                                                             .parameters{ std::move(parameters) },
                                                                             .return_type{ return_type },
                                                                             .body{ std::move(body) } });

            return function_definition;
        }

        [[nodiscard]] Statements::Block block() {
            Statements::StatementList statements;
            consume<LeftCurlyBracket>("expected \"{\"");
            while (not end_of_file() and not current_is<RightCurlyBracket>()) {
                if (current_is<Let>()) {
                    statements.push_back(variable_definition());
                } else if (current_is<LeftCurlyBracket>()) {
                    statements.push_back(std::make_unique<Statements::Block>(block()));
                } else {
                    Error::error(
                            current(), std::format(
                                               "unexpected token: \"{}\"",
                                               std::visit([](auto&& token) { return token.debug_name; }, current())
                                       )
                    );
                    advance();
                }
            }
            consume<RightCurlyBracket>("expected \"}\"");
            return Statements::Block{ std::move(statements) };
        }

        [[nodiscard]] Type type() {
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
            const auto variable_type = type();
            consume<Equals>("expected variable initialization");
            auto initial_value = expression();
            consume<Semicolon>("expected \";\"");
            return std::make_unique<Statements::VariableDefinition>(
                    identifier, variable_type, std::move(initial_value)
            );
        }

        template<typename T>
        [[nodiscard]] bool current_is() const {
            return std::holds_alternative<T>(current());
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
