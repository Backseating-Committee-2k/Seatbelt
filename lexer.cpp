//
// Created by coder2k on 20.05.2022.
//

#include "lexer.hpp"
#include <array>
#include <cassert>
#include <ctre.hpp>
#include <fmt/core.h>
#include <iostream>

class LexerState {
public:
    explicit LexerState(SourceCode source_code) : m_source_code{ source_code } { }

    [[nodiscard]] Lexer::TokenList tokenize() {
        using namespace Lexer;
        using namespace Lexer::Tokens;

        TokenList tokens;

        while (not end_of_file()) {
            usize token_length = 1;
            switch (current()) {
                case '+':
                    emit_single_char_token<Plus>(tokens);
                    break;
                case '-':
                    if (peek() == '>') {
                        token_length = 2;
                        emit_token<Arrow>(tokens, token_length);
                    } else {
                        emit_single_char_token<Minus>(tokens);
                    }
                    break;
                case '*':
                    emit_single_char_token<Asterisk>(tokens);
                    break;
                case '/':
                    if (peek() == '/') {
                        while (not end_of_file() && current() != '\n') {
                            advance(1);
                        }
                        continue;
                    } else if (peek() == '*') {
                        usize last_opening_block_comment_index = m_index;
                        i64 nesting_depth = 1;
                        advance(2);
                        while ([[maybe_unused]] bool two_remaining = m_index < m_source_code.text.length() - 1) {
                            if (current() == '/' and peek() == '*') {
                                last_opening_block_comment_index = m_index;
                                ++nesting_depth;
                                advance(2);
                            } else if (current() == '*' and peek() == '/') {
                                --nesting_depth;
                                advance(1);
                            } else {
                                advance(1);
                            }
                            if (nesting_depth == 0) {
                                break;
                            }
                        }
                        if (nesting_depth != 0) {
                            fmt::print(
                                    stderr, "unclosed block comment: {}\n",
                                    m_source_code.text.substr(last_opening_block_comment_index)
                            );
                            std::exit(EXIT_FAILURE);
                        }
                    } else {
                        emit_single_char_token<ForwardSlash>(tokens);
                    }
                    break;
                case '%':
                    emit_single_char_token<Percent>(tokens);
                    break;
                case '(':
                    emit_single_char_token<LeftParenthesis>(tokens);
                    break;
                case ')':
                    emit_single_char_token<RightParenthesis>(tokens);
                    break;
                case ';':
                    emit_single_char_token<Semicolon>(tokens);
                    break;
                case '{':
                    emit_single_char_token<LeftCurlyBracket>(tokens);
                    break;
                case '}':
                    emit_single_char_token<RightCurlyBracket>(tokens);
                    break;
                case ':':
                    emit_single_char_token<Colon>(tokens);
                    break;
                case ',':
                    emit_single_char_token<Comma>(tokens);
                    break;
                case '=':
                    emit_single_char_token<Equals>(tokens);
                    break;
                default:
                    const auto c = current();
                    if (std::isspace(current())) {
                        break;
                    }
                    std::string_view remaining_source = m_source_code.text.substr(m_index);
                    if (const auto identifier_result =
                                ctre::starts_with<"([a-zA-Z_][a-zA-Z0-9_]*)">(remaining_source)) {
                        token_length = identifier_result.view().length();
                        if (identifier_result.view() == "dump_registers") {
                            emit_token<DumpRegisters>(tokens, token_length);
                        } else if (identifier_result.view() == "function") {
                            emit_token<Function>(tokens, token_length);
                        } else if (identifier_result.view() == "let") {
                            emit_token<Let>(tokens, token_length);
                        } else if (identifier_result.view() == "bsm") {
                            token_length = inline_assembly(tokens);
                        } else {
                            emit_token<Identifier>(tokens, token_length);
                        }
                    } else if (const auto integer_literal_result = ctre::starts_with<"(0o([0-7]+_?)+)|(0x([\\dA-Fa-f]+_?)+)|(0b([01]+_?)+)|(\\d+_?)+">(remaining_source)) {
                        token_length = integer_literal_result.view().length();
                        emit_token<IntegerLiteral>(tokens, token_length);
                    } else {
                        std::cerr << "unexpected input: " << remaining_source << "\n";
                        std::exit(EXIT_FAILURE);
                    }

                    break;
            }
            advance(token_length);
        }

        tokens.push_back(EndOfFile{ .location{ location(1) } });

        return tokens;
    }

private:
    [[nodiscard]] char current() const {
        return m_source_code.text[m_index];
    }

    /// Returns the next character or the null character, if the the access would
    /// be out of bounds.
    [[nodiscard]] char peek() const {
        return m_index + 1 == m_source_code.text.length() ? char{ 0 } : m_source_code.text[m_index + 1];
    }

    [[nodiscard]] bool end_of_file() const {
        assert(m_index <= m_source_code.text.length());
        return m_index >= m_source_code.text.length();
    }

    [[nodiscard]] char consume() {
        return m_source_code.text[m_index++];
    }

    void skip() {
        advance(1);
    }

    void advance(usize amount) {
        m_index += amount;
    }

    [[nodiscard]] Location location(const usize length) const {
        return Location{ .source_code{ m_source_code },
                         .offset_start_inclusive{ m_index },
                         .offset_end_exclusive{ m_index + length } };
    }

    [[nodiscard]] usize inline_assembly(Lexer::TokenList& tokens) {
        const usize inline_assembly_start = m_index;

        // discard bsm keyword
        advance(3);

        // discard any following whitespace after the "bsm" keyword
        while (not end_of_file() and std::isspace(current())) {
            advance(1);
        }

        if (end_of_file()) {
            std::cerr << "unexpected end of file\n";
            std::exit(EXIT_FAILURE);
        }

        if (current() != '{') {
            std::cerr << "expected \"{\" to open an inline assembly block\n";
            std::exit(EXIT_FAILURE);
        }

        advance(1);

        while (not end_of_file() and current() != '}') {
            if (current() == '/') {
                advance(1);
                if (not end_of_file() and current() == '/') {
                    // we are inside a comment
                    // loop until end of line
                    advance(1);
                    while (not end_of_file() and current() != '\n') {
                        advance(1);
                    }
                }
            } else {
                advance(1);
            }
        }
        if (end_of_file()) {
            std::cerr << "unterminated inline assembly block\n";
            std::exit(EXIT_FAILURE);
        }

        assert(current() == '}');
        advance(1);
        const usize inline_assembly_end = m_index;
        tokens.push_back(Lexer::Tokens::InlineAssembly{
                .location{.source_code{ m_source_code },
                          .offset_start_inclusive{ inline_assembly_start },
                          .offset_end_exclusive{ inline_assembly_end }}
        });
        const usize token_length = inline_assembly_end - inline_assembly_start;
        assert(m_index >= token_length);
        m_index -= token_length;
        return token_length;
    }

    template<typename T, typename... Args>
    void emit_token(Lexer::TokenList& tokens, const usize token_length, Args... args) {
        tokens.push_back(T{ location(token_length), args... });
    }

    template<typename T, typename... Args>
    void emit_single_char_token(Lexer::TokenList& tokens, Args... args) {
        emit_token<T>(tokens, 1, args...);
    }

private:
    SourceCode m_source_code;
    usize m_index{ 0 };
};

namespace Lexer {

    TokenList tokenize(const SourceCode source_code) {
        return LexerState{ source_code }.tokenize();
    }

}// namespace Lexer
