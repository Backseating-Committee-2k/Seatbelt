//
// Created by coder2k on 13.07.2022.
//

#pragma once

#include "lexer.hpp"
#include "mutability.hpp"
#include <memory>
#include <span>
#include <vector>

struct DataType;

class TypeContainer {
public:
    TypeContainer();

    [[nodiscard]] const DataType* from_type_definition(std::unique_ptr<DataType> data_type);
    void register_type(std::unique_ptr<DataType> data_type);
    [[nodiscard]] bool is_defined(const DataType& data_type) const;
    [[nodiscard]] const DataType* pointer_to(const DataType* pointee_type, Mutability binding_mutability);
    [[nodiscard]] const DataType* array_of(const DataType* contained, usize num_elements);

    [[nodiscard]] const DataType* get_u32() const;
    [[nodiscard]] const DataType* get_bool() const;
    [[nodiscard]] const DataType* get_char() const;
    [[nodiscard]] const DataType* get_nothing() const;

private:
    [[nodiscard]] const DataType* find(const DataType& data_type) const;

private:
    std::vector<std::unique_ptr<DataType>> m_data_types;
    const DataType* m_u32;
    const DataType* m_bool;
    const DataType* m_char;
    const DataType* m_nothing;
};
