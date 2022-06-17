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
#include "parser.hpp"

namespace Parser {

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
        [[nodiscard]] FunctionDefinition function() {
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
            return FunctionDefinition{
                .name{ name },
                .parameters{ std::move(parameters) },
                .return_type{ return_type },
                .body{ std::move(body) }
            };
        }

        Block block() {
            StatementList statements;
            consume<LeftCurlyBracket>("expected \"{\"");
            while (not end_of_file() and not current_is<RightCurlyBracket>()) {
                if (current_is<Let>()) {
                    statements.push_back(variable_definition());
                } else if (current_is<LeftCurlyBracket>()) {
                    block();
                } else {
                    advance();
                }
            }
            consume<RightCurlyBracket>("expected \"}\"");
            return Block{ std::move(statements) };
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

        std::unique_ptr<VariableDefinition> variable_definition() {
            assert(current_is<Let>());
            advance();
            const auto identifier = consume<Identifier>("expected variable name");
            consume<Colon>("expected \":\"");
            const auto variable_type = type();
            consume<Equals>("expected variable initialization");
            const auto initial_value = consume<IntegerLiteral>("FNLKJDSFHLKSDFGJNLKFDSGN EXPRESSSSIOOOOON!");
            consume<Semicolon>("expected \";\"");
            return std::make_unique<VariableDefinition>(identifier, variable_type, initial_value);
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

        [[nodiscard]] static Location token_location(const auto& token) {
            return std::visit([](const auto& token) { return token.location; }, token);
        }

        void error(const std::string_view message) const {
            error(current(), message);
        }

        static void error(const Token& token, const std::string_view message) {
            using namespace std::ranges::views;
            using std::ranges::count, std::ranges::find;
            const auto npos = std::string_view::npos;

            const auto location = token_location(token);
            const auto row = count(location.source_code.text | take(location.offset_start_inclusive), '\n') + 1;
            const auto last_newline_pos = location.source_code.text.find_last_of('\n', location.offset_start_inclusive);
            const auto column = location.offset_start_inclusive - (last_newline_pos == npos ? -1 : last_newline_pos);

            const auto line_start_pos = last_newline_pos == npos ? 0 : last_newline_pos + 1;
            const auto next_newline_pos = location.source_code.text.find('\n', line_start_pos);
            const auto line_end_pos = next_newline_pos == npos ? location.source_code.text.length() : next_newline_pos;
            const auto line = location.source_code.text.substr(line_start_pos, line_end_pos - line_start_pos);

            std::cerr << std::format("{}:{}:{}: {}\n{}\n", location.source_code.filename, row, column, message, line);
            for (usize i = 0; i < column - 1; ++i) {
                std::cerr << ' ';
            }
            std::cerr << '^';
            const auto squiggly_length = std::max(location.view().length(), usize{ 1 }) - 1;
            for (usize i = 0; i < squiggly_length; ++i) {
                std::cerr << '~';
            }
            std::cerr << " error occurred here\n";
            std::exit(EXIT_FAILURE);
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
