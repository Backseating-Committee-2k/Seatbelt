//
// Created by coder2k on 13.07.2022.
//

#include "type_container.hpp"
#include "data_type.hpp"
#include "error.hpp"
#include <algorithm>
#include <ranges>

TypeContainer::TypeContainer() {
    m_u32 = from_type_definition(std::make_unique<ConcreteType>(U32Identifier));
    m_char = from_type_definition(std::make_unique<ConcreteType>(CharIdentifier));
    m_bool = from_type_definition(std::make_unique<ConcreteType>(BoolIdentifier));
    m_nothing = from_type_definition(std::make_unique<ConcreteType>(NothingIdentifier));
}

[[nodiscard]] const DataType* TypeContainer::from_type_definition(std::unique_ptr<DataType> type_definition) {
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
bool TypeContainer::is_defined(const DataType& data_type) const {
    if (const auto concrete_type = dynamic_cast<const ConcreteType*>(&data_type)) {
        return find(data_type) != nullptr;
    }
    if (const auto pointer_type = dynamic_cast<const PointerType*>(&data_type)) {
        return is_defined(*pointer_type->contained);
    }
    if (const auto function_pointer_type = dynamic_cast<const FunctionPointerType*>(&data_type)) {
        for (const auto& parameter_type : function_pointer_type->parameter_types) {
            if (not is_defined(*parameter_type)) {
                return false;
            }
        }
        return is_defined(*function_pointer_type->return_type);
    }
    assert(false and "not implemented");
    return false;
}

[[nodiscard]] const DataType* TypeContainer::pointer_to(const DataType* pointee_type, Mutability binding_mutability) {
    return from_type_definition(std::make_unique<PointerType>(pointee_type, binding_mutability));
}

const DataType* TypeContainer::find(const DataType& data_type) const {
    using std::ranges::find_if;
    const auto find_iterator = find_if(m_data_types, [&](const auto& other) { return data_type == *other; });
    const auto data_type_found = (find_iterator != std::cend(m_data_types));
    return data_type_found ? find_iterator->get() : nullptr;
}

const DataType* TypeContainer::get_u32() const {
    return m_u32;
}

const DataType* TypeContainer::get_bool() const {
    return m_bool;
}

const DataType* TypeContainer::get_char() const {
    return m_char;
}

const DataType* TypeContainer::get_nothing() const {
    return m_nothing;
}
