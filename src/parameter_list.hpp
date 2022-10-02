//
// Created by coder2k on 14.07.2022.
//

#pragma once

#include "data_type.hpp"
#include "lexer.hpp"
#include <span>

struct VariableSymbol;

struct Parameter {
    Lexer::Tokens::Identifier name;
    std::unique_ptr<DataType> type_definition;
    Mutability binding_mutability;
    DataType* data_type{ nullptr };
    VariableSymbol* variable_symbol{ nullptr };
};

using ParameterList = std::vector<Parameter>;
