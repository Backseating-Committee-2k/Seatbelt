//
// Created by coder2k on 20.05.2022.
//

#pragma once

#include <string_view>
#include <variant>
#include <vector>
#include "types.hpp"
#include "location.hpp"

namespace Lexer {

    namespace Tokens {

        struct DumpRegisters {
            Location location;
            static constexpr const char* debug_name = "DumpRegisters";
        };

        struct Identifier {
            Location location;
            static constexpr const char* debug_name = "Identifier";
        };

        struct Function {
            Location location;
            static constexpr const char* debug_name = "Function";
        };

        struct Colon {
            Location location;
            static constexpr const char* debug_name = "Colon";
        };

        struct Comma {
            Location location;
            static constexpr const char* debug_name = "Comma";
        };

        struct Arrow {
            Location location;
            static constexpr const char* debug_name = "Arrow";
        };

        struct EndOfFile {
            Location location;
            static constexpr const char* debug_name = "EndOfFile";
        };

        struct Semicolon {
            Location location;
            static constexpr const char* debug_name = "Semicolon";
        };

        struct Plus {
            Location location;
            static constexpr const char* debug_name = "Plus";
        };

        struct Minus {
            Location location;
            static constexpr const char* debug_name = "Minus";
        };

        struct Asterisk {
            Location location;
            static constexpr const char* debug_name = "Asterisk";
        };

        struct Percent {
            Location location;
            static constexpr const char* debug_name = "Percent";
        };

        struct ForwardSlash {
            Location location;
            static constexpr const char* debug_name = "ForwardSlash";
        };

        struct DoubleForwardSlash {
            Location location;
            static constexpr const char* debug_name = "DoubleForwardSlash";
        };

        struct ForwardSlashAsterisk {
            Location location;
            static constexpr const char* debug_name = "ForwardSlashAsterisk";
        };

        struct AsteriskForwardSlash {
            Location location;
            static constexpr const char* debug_name = "AsteriskForwardSlash";
        };

        struct LeftParenthesis {
            Location location;
            static constexpr const char* debug_name = "LeftParenthesis";
        };

        struct RightParenthesis {
            Location location;
            static constexpr const char* debug_name = "RightParenthesis";
        };

        struct LeftCurlyBracket {
            Location location;
            static constexpr const char* debug_name = "LeftCurlyBracket";
        };

        struct RightCurlyBracket {
            Location location;
            static constexpr const char* debug_name = "RightCurlyBracket";
        };

        struct Let {
            Location location;
            static constexpr const char* debug_name = "Let";
        };

        struct Equals {
            Location location;
            static constexpr const char* debug_name = "Equals";
        };

        struct IntegerLiteral {
            Location location;
            static constexpr const char* debug_name = "IntegerLiteral";
        };

        using Token = std::variant<
                DumpRegisters,
                Identifier,
                Function,
                Colon,
                Comma,
                Arrow,
                EndOfFile,
                Semicolon,
                Plus,
                Minus,
                Asterisk,
                ForwardSlash,
                Percent,
                LeftParenthesis,
                RightParenthesis,
                DoubleForwardSlash,
                ForwardSlashAsterisk,
                AsteriskForwardSlash,
                LeftCurlyBracket,
                RightCurlyBracket,
                Let,
                Equals,
                IntegerLiteral>;

    }// namespace Tokens

    using TokenList = std::vector<Tokens::Token>;

    [[nodiscard]] TokenList tokenize(SourceCode source_code);

}// namespace Lexer
