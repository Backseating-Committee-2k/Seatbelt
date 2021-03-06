//
// Created by coder2k on 24.06.2022.
//

#pragma once

#include "lexer.hpp"
#include <string_view>

namespace Error {

    void error(const Lexer::Tokens::Token& token, std::string_view message);
    void warning(const Lexer::Tokens::Token& token, std::string_view message);

    [[nodiscard]] Location token_location(const auto& token) {
        return std::visit([](const auto& token) { return token.location; }, token);
    }

}// namespace Error
