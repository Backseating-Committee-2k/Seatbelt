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

// clang-format off
#define TOKEN_LIST \
    x(DumpRegisters) \
    x(Identifier) \
    x(Function) \
    x(Colon) \
    x(Comma) \
    x(Arrow) \
    x(TildeArrow) \
    x(EndOfFile) \
    x(Semicolon) \
    x(Plus) \
    x(Minus) \
    x(Asterisk) \
    x(Mod) \
    x(ForwardSlash) \
    x(LeftParenthesis) \
    x(RightParenthesis) \
    x(LeftCurlyBracket) \
    x(RightCurlyBracket) \
    x(Let) \
    x(Equals) \
    x(IntegerLiteral) \
    x(CharLiteral) \
    x(BoolLiteral) \
    x(InlineAssembly) \
    x(Import) \
    x(Dot) \
    x(DoubleColon) \
    x(Namespace) \
    x(And) \
    x(Or) \
    x(Not) \
    x(Xor) \
    x(If) \
    x(Else) \
    x(Loop) \
    x(Break) \
    x(Continue) \
    x(While) \
    x(Do) \
    x(For) \
    x(Mutable) \
    x(Const) \
    x(EqualsEquals) \
    x(ExclamationEquals) \
    x(GreaterThan) \
    x(LessThan) \
    x(GreaterOrEquals) \
    x(LessOrEquals) \
    x(Return) \
    x(Label) \
    x(Goto) \
    x(NothingLiteral) \
    x(CapitalizedFunction) \
    x(Export)

// clang-format on


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

template<typename T>
[[nodiscard]] bool is(const Lexer::Tokens::Token& token) {
    return std::holds_alternative<T>(token);
}

template<typename... T>
[[nodiscard]] bool is_one_of(const Lexer::Tokens::Token& token) {
    return (is<T>(token) or ...);
}

#undef TOKEN_LIST
#undef DEFINE_TOKEN
