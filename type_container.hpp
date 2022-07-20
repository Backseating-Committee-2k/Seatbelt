//
// Created by coder2k on 13.07.2022.
//

#pragma once

#include "data_type.hpp"
#include "lexer.hpp"
#include <span>
#include <vector>

class TypeContainer {
public:
    [[nodiscard]] const DataType*
    from_tokens(std::span<const Lexer::Tokens::Token> type_tokens, bool is_mutable = false);

    [[nodiscard]] const DataType* from_data_type(std::unique_ptr<DataType> data_type);

    [[nodiscard]] const DataType* remove_const(const DataType* data_type);

private:
    std::vector<std::unique_ptr<DataType>> m_data_types;
};
