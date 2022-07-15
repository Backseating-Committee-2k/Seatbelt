//
// Created by coder2k on 20.05.2022.
//

#pragma once

#include "location.hpp"
#include "types.hpp"
#include <string_view>
#include <variant>
#include <vector>

#define DEFINE_TOKEN(name)                               \
    struct name {                                        \
        Location location;                               \
        static constexpr const char* debug_name = #name; \
    };

#define TOKEN_LIST                                                                                                   \
    x(DumpRegisters) x(Identifier) x(Function) x(Colon) x(Comma) x(Arrow) x(EndOfFile) x(Semicolon) x(Plus) x(Minus) \
            x(Asterisk) x(Percent) x(ForwardSlash) x(LeftParenthesis) x(RightParenthesis) x(LeftCurlyBracket)        \
                    x(RightCurlyBracket) x(Let) x(Equals) x(IntegerLiteral) x(InlineAssembly) x(Import) x(Dot)


namespace Lexer {

    namespace Tokens {

#define x(TokenType) DEFINE_TOKEN(TokenType)
        TOKEN_LIST
#undef x

        template<typename Head, typename... Tail>
        std::variant<Tail...> cut_head_off_(std::variant<Head, Tail...>);

#define x(TokenType) , TokenType
        using Token = decltype(cut_head_off_(std::declval<std::variant<std::monostate TOKEN_LIST>>()));
#undef x
    }// namespace Tokens

    using TokenList = std::vector<Tokens::Token>;

    [[nodiscard]] TokenList tokenize(SourceCode source_code);

}// namespace Lexer

#undef TOKEN_LIST
#undef DEFINE_TOKEN
