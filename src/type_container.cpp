//
// Created by coder2k on 13.07.2022.
//

#include "type_container.hpp"
#include "data_type.hpp"
#include "error.hpp"
#include <algorithm>
#include <ranges>

TypeContainer::TypeContainer() {
    m_u32 = from_type_definition(std::make_unique<PrimitiveType>(BasicType::U32));
    m_char = from_type_definition(std::make_unique<PrimitiveType>(BasicType::Char));
    m_bool = from_type_definition(std::make_unique<PrimitiveType>(BasicType::Bool));
    m_nothing = from_type_definition(std::make_unique<PrimitiveType>(BasicType::Nothing));
}

[[nodiscard]] DataType* TypeContainer::from_type_definition(std::unique_ptr<DataType> type_definition) {
    const auto found = find(*type_definition);
    if (found != nullptr) {
        return found;
    } else {
        m_data_types.push_back(std::move(type_definition));
        return m_data_types.back().get();
    }
}

void TypeContainer::register_type(std::unique_ptr<DataType> data_type) {
    static_cast<void>(from_type_definition(std::move(data_type)));
}

/**
 * This function checks if a data type is already defined. A data type is defined if it either
 * * is a type that is already known.
 * * is a pointer to a type that is defined.
 * * is a function pointer type with all its parameter types and return type defined.
 * @param data_type The data type to check.
 * @return `true` if the type is defined, `false` otherwise.
 */
bool TypeContainer::is_defined(DataType& data_type) const {
    if (const auto primitive_type = dynamic_cast<const PrimitiveType*>(&data_type)) {
        const auto find_result = find(data_type);
        const auto found = (find_result != nullptr);
        return found;
    }
    if (const auto pointer_type = dynamic_cast<PointerType*>(&data_type)) {
        return is_defined(*(pointer_type->contained));
    }
    if (const auto function_pointer_type = dynamic_cast<FunctionPointerType*>(&data_type)) {
        for (const auto& parameter_type : function_pointer_type->parameter_types) {
            if (not is_defined(*parameter_type)) {
                return false;
            }
        }
        return is_defined(*function_pointer_type->return_type);
    }
    if (const auto array_type = dynamic_cast<ArrayType*>(&data_type)) {
        return is_defined(*(array_type->contained));
    }
    if (const auto struct_type = dynamic_cast<StructType*>(&data_type)) {
        using std::ranges::all_of, std::ranges::views::transform;
        return all_of(
                struct_type->members | transform([](const auto& attribute) { return attribute.data_type; }),
                [&](const auto& data_type) { return is_defined(*data_type); }
        );
    }
    [[maybe_unused]] const auto placeholder_type = dynamic_cast<const CustomTypePlaceholder*>(&data_type);
    assert(placeholder_type == nullptr and "placeholders should have been replaced by now");
    assert(false and "not implemented");
    return false;
}

[[nodiscard]] DataType* TypeContainer::function_pointer(std::vector<DataType*> parameter_types, DataType* return_type) {
    return from_type_definition(std::make_unique<FunctionPointerType>(std::move(parameter_types), return_type));
}

[[nodiscard]] DataType* TypeContainer::pointer_to(DataType* pointee_type, Mutability binding_mutability) {
    return from_type_definition(std::make_unique<PointerType>(pointee_type, binding_mutability));
}

[[nodiscard]] DataType*
TypeContainer::struct_of(std::string name, std::string namespace_qualifier, std::vector<StructMember> attributes) {
    return from_type_definition(
            std::make_unique<StructType>(std::move(name), std::move(namespace_qualifier), std::move(attributes))
    );
}

[[nodiscard]] DataType* TypeContainer::array_of(DataType* contained, usize num_elements) {
    return from_type_definition(std::make_unique<ArrayType>(contained, num_elements));
}

[[nodiscard]] DataType* TypeContainer::find(DataType& data_type) const {
    using std::ranges::find_if;
    const auto find_iterator = find_if(m_data_types, [&](const auto& other) { return data_type == *other; });
    const auto data_type_found = (find_iterator != std::cend(m_data_types));
    return data_type_found ? find_iterator->get() : nullptr;
}

[[nodiscard]] DataType* TypeContainer::get_u32() const {
    return m_u32;
}

[[nodiscard]] DataType* TypeContainer::get_bool() const {
    return m_bool;
}

[[nodiscard]] DataType* TypeContainer::get_char() const {
    return m_char;
}

[[nodiscard]] DataType* TypeContainer::get_nothing() const {
    return m_nothing;
}
