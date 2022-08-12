//
// Created by coder2k on 24.06.2022.
//

#pragma once

#include "lexer.hpp"
#include <string_view>

namespace Parser::Statements {
    struct Statement;
}

namespace Parser::Expressions {
    struct Expression;
};

namespace Error {

    void error(const Lexer::Tokens::Token& token, std::string_view message);
    void warning(const Lexer::Tokens::Token& token, std::string_view message);
    void error(const Parser::Statements::Statement& statement, std::string_view message);
    void warning(const Parser::Statements::Statement& statement, std::string_view message);
    void error(const Parser::Expressions::Expression& expression, std::string_view message);
    void warning(const Parser::Expressions::Expression& expression, std::string_view message);

    [[nodiscard]] Location token_location(const auto& token) {
        return std::visit([](const auto& token) { return token.location; }, token);
    }

}// namespace Error
