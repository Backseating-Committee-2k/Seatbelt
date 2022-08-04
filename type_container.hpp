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
    TypeContainer();

    [[nodiscard]] const DataType* from_type_definition(std::unique_ptr<DataType> data_type);
    void register_type(std::unique_ptr<DataType> data_type);
    [[nodiscard]] bool is_defined(const std::unique_ptr<DataType>& data_type) const;

private:
    [[nodiscard]] const DataType* find(const std::unique_ptr<DataType>& data_type) const;

private:
    std::vector<std::unique_ptr<DataType>> m_data_types;
};
