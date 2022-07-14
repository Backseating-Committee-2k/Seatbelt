//
// Created by coder2k on 14.07.2022.
//

#pragma once

#include "data_type.hpp"
#include "lexer.hpp"
#include <span>


struct Parameter {
    Lexer::Tokens::Identifier name;
    std::span<const Lexer::Tokens::Token> type_tokens;
    const DataType* type{ nullptr };
};

using ParameterList = std::vector<Parameter>;
