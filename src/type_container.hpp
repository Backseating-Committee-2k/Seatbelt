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

    [[nodiscard]] const DataType* const_u32() const;
    [[nodiscard]] const DataType* const_bool() const;
    [[nodiscard]] const DataType* const_char() const;
    [[nodiscard]] const DataType* const_nothing() const;
    [[nodiscard]] const DataType* mutable_nothing() const;

private:
    [[nodiscard]] const DataType* find(const std::unique_ptr<DataType>& data_type) const;

private:
    std::vector<std::unique_ptr<DataType>> m_data_types;
    const DataType* m_const_u32;
    const DataType* m_const_bool;
    const DataType* m_const_char;
    const DataType* m_const_nothing;
    const DataType* m_mutable_nothing;
};
