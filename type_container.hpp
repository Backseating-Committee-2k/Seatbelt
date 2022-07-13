//
// Created by coder2k on 13.07.2022.
//

#pragma once

#include <vector>
#include <span>
#include "data_type.hpp"
#include "lexer.hpp"

class TypeContainer {
public:
    [[nodiscard]] const DataType* from_tokens(std::span<const Lexer::Tokens::Token> type_tokens);
    [[nodiscard]] const DataType* from_data_type(std::unique_ptr<DataType> data_type);

private:
    std::vector<std::unique_ptr<DataType>> m_data_types;
};
