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
struct StructMember;

class TypeContainer {
public:
    TypeContainer();

    [[nodiscard]] DataType* from_type_definition(std::unique_ptr<DataType> data_type);
    void register_type(std::unique_ptr<DataType> data_type);
    [[nodiscard]] bool is_defined(DataType& data_type) const;
    [[nodiscard]] DataType* function_pointer(std::vector<DataType*> parameter_types, DataType* return_type);
    [[nodiscard]] DataType* struct_of(
            std::string name,
            std::string namespace_qualifier,
            std::vector<StructMember> attributes
    );
    [[nodiscard]] DataType* pointer_to(DataType* pointee_type, Mutability binding_mutability);
    [[nodiscard]] DataType* array_of(DataType* contained, usize num_elements);

    [[nodiscard]] DataType* get_u32() const;
    [[nodiscard]] DataType* get_bool() const;
    [[nodiscard]] DataType* get_char() const;
    [[nodiscard]] DataType* get_nothing() const;

private:
    [[nodiscard]] DataType* find(DataType& data_type) const;

private:
    std::vector<std::unique_ptr<DataType>> m_data_types;
    DataType* m_u32;
    DataType* m_bool;
    DataType* m_char;
    DataType* m_nothing;
};
