//
// Created by coder2k on 20.05.2022.
//

#include "lexer.hpp"
#include <array>
#include <cassert>
#include <ctre.hpp>
#include <fmt/core.h>
#include <iostream>


#define KEYWORD(keyword, type)                 \
    if (identifier_result.view() == keyword) { \
        emit_token<type>(tokens, length);      \
        return length;                         \
    }

#define KEYWORDS(first_keyword, second_keyword, type)                                                   \
    else if (identifier_result.view() == first_keyword or identifier_result.view() == second_keyword) { \
        emit_token<type>(tokens, length);                                                               \
        return length;                                                                                  \
    }

#define SINGLE_CHAR_TOKEN(single_char, token_type)  \
    case single_char:                               \
        emit_single_char_token<token_type>(tokens); \
        break;

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
                SINGLE_CHAR_TOKEN('.', Dot)
                SINGLE_CHAR_TOKEN('+', Plus)
                SINGLE_CHAR_TOKEN('*', Asterisk)
                SINGLE_CHAR_TOKEN('%', Percent)
                SINGLE_CHAR_TOKEN('(', LeftParenthesis)
                SINGLE_CHAR_TOKEN(')', RightParenthesis)
                SINGLE_CHAR_TOKEN(';', Semicolon)
                SINGLE_CHAR_TOKEN('{', LeftCurlyBracket)
                SINGLE_CHAR_TOKEN('}', RightCurlyBracket)
                SINGLE_CHAR_TOKEN(',', Comma)
                case '>':
                    token_length = greater_than(tokens);
                    break;
                case '<':
                    token_length = less_than(tokens);
                    break;
                case '=':
                    token_length = equals(tokens);
                    break;
                case '-':
                    token_length = minus(tokens);
                    break;
                case '/':
                    token_length = forward_slash(tokens);
                    break;
                case ':':
                    token_length = colon(tokens);
                    break;
                default:
                    token_length = other_multichar_token(tokens);
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

    [[nodiscard]] usize greater_than(Lexer::TokenList& tokens) {
        assert(current() == '>');
        using namespace Lexer::Tokens;
        if (peek() == '=') {
            emit_token<GreaterOrEquals>(tokens, 2);
            return 2;
        }
        emit_single_char_token<GreaterThan>(tokens);
        return 1;
    }

    [[nodiscard]] usize less_than(Lexer::TokenList& tokens) {
        assert(current() == '<');
        using namespace Lexer::Tokens;
        if (peek() == '=') {
            emit_token<LessOrEquals>(tokens, 2);
            return 2;
        }
        emit_single_char_token<LessThan>(tokens);
        return 1;
    }

    [[nodiscard]] usize equals(Lexer::TokenList& tokens) {
        using namespace Lexer::Tokens;
        assert(current() == '=');
        if (peek() == '=') {
            emit_token<EqualsEquals>(tokens, 2);
            return 2;
        }
        emit_single_char_token<Equals>(tokens);
        return 1;
    }

    [[nodiscard]] usize minus(Lexer::TokenList& tokens) {
        using namespace Lexer::Tokens;
        assert(current() == '-');
        if (peek() == '>') {
            emit_token<Arrow>(tokens, 2);
            return 2;
        }
        emit_single_char_token<Minus>(tokens);
        return 1;
    }

    [[nodiscard]] usize single_line_comment() {
        assert(current() == '/' and peek() == '/');
        while (not end_of_file() && current() != '\n') {
            advance(1);
        }
        return 0;
    }

    [[nodiscard]] usize block_comment() {
        assert(current() == '/' and peek() == '*');
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
                    stderr, "unclosed block comment: {}\n", m_source_code.text.substr(last_opening_block_comment_index)
            );
            std::exit(EXIT_FAILURE);
        }
        return 1;
    }

    [[nodiscard]] usize forward_slash(Lexer::TokenList& tokens) {
        assert(current() == '/');
        switch (peek()) {
            case '/':
                return single_line_comment();
            case '*':
                return block_comment();
            default:
                emit_single_char_token<Lexer::Tokens::ForwardSlash>(tokens);
                return 1;
        }
    }

    [[nodiscard]] usize colon(Lexer::TokenList& tokens) {
        if (peek() == ':') {
            emit_token<Lexer::Tokens::DoubleColon>(tokens, 2);
            return 2;
        }
        emit_single_char_token<Lexer::Tokens::Colon>(tokens);
        return 1;
    }

    [[nodiscard]] usize other_multichar_token(Lexer::TokenList& tokens) {
        using namespace Lexer::Tokens;
        if (std::isspace(current())) {
            return 1;
        }

        std::string_view remaining_source = m_source_code.text.substr(m_index);

        if (remaining_source.starts_with("!=")) {
            emit_token<ExclamationEquals>(tokens, 2);
            return 2;
        }

        static constexpr char identifier_pattern[] = "([a-zA-Z_][a-zA-Z0-9_]*)";
        static constexpr char integer_pattern[] = "(0o([0-7]+_?)+)|(0x([\\dA-Fa-f]+_?)+)|(0b([01]+_?)+)|(\\d+_?)+";
        static constexpr char char_pattern[] = R"('(\\'|[ -~]|\\[n\\tnvfr0])')";

        if (const auto char_literal_result = ctre::starts_with<char_pattern>(remaining_source)) {
            const usize length = char_literal_result.view().length();
            emit_token<CharLiteral>(tokens, length);
            return length;
        } else if (const auto integer_literal_result = ctre::starts_with<integer_pattern>(remaining_source)) {
            const usize length = integer_literal_result.view().length();
            emit_token<IntegerLiteral>(tokens, length);
            return length;
        } else if (const auto identifier_result = ctre::starts_with<identifier_pattern>(remaining_source)) {
            const usize length = identifier_result.view().length();
            KEYWORD("dump_registers", DumpRegisters)
            KEYWORD("function", Function)
            KEYWORD("import", Import)
            KEYWORD("let", Let)
            KEYWORD("namespace", Namespace)
            KEYWORDS("true", "false", BoolLiteral)
            KEYWORD("and", And)
            KEYWORD("or", Or)
            KEYWORD("not", Not)
            KEYWORD("xor", Xor)
            KEYWORD("if", If)
            KEYWORD("else", Else)
            KEYWORD("loop", Loop)
            KEYWORD("break", Break)
            KEYWORD("continue", Continue)
            KEYWORD("while", While)
            KEYWORD("do", Do)
            KEYWORD("for", For)
            KEYWORD("mutable", Mutable)
            KEYWORD("const", Const)
            KEYWORD("return", Return)
            KEYWORD("label", Label)
            KEYWORD("goto", Goto)
            KEYWORD("nothing", NothingLiteral)
            if (identifier_result.view() == "bsm") {
                return inline_assembly(tokens);
            }
            emit_token<Identifier>(tokens, length);
            return length;
        }
        std::cerr << "unexpected input: " << remaining_source << "\n";
        std::exit(EXIT_FAILURE);
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
