//
// Created by coder2k on 10.07.2022.
//

#pragma once

#include "parser.hpp"
#include <memory>

namespace TypeChecker {
    [[nodiscard]] DataTypePointer tokens_to_type(std::span<const Lexer::Tokens::Token> tokens);

    void check(const Parser::Program& program);
}// namespace TypeChecker
