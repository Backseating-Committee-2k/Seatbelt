//
// Created by coder2k on 13.07.2022.
//

#include "type_container.hpp"
#include "data_type.hpp"
#include "error.hpp"
#include <algorithm>
#include <ranges>

TypeContainer::TypeContainer() {
    register_type(std::make_unique<ConcreteType>(U32Identifier, true));
    m_const_u32 = from_type_definition(std::make_unique<ConcreteType>(U32Identifier, false));
    register_type(std::make_unique<ConcreteType>(CharIdentifier, true));
    m_const_char = from_type_definition(std::make_unique<ConcreteType>(CharIdentifier, false));
    register_type(std::make_unique<ConcreteType>(BoolIdentifier, true));
    m_const_bool = from_type_definition(std::make_unique<ConcreteType>(BoolIdentifier, false));
    m_mutable_nothing = from_type_definition(std::make_unique<ConcreteType>(NothingIdentifier, true));
    m_const_nothing = from_type_definition(std::make_unique<ConcreteType>(NothingIdentifier, false));
}

[[nodiscard]] static bool are_types_equal(const std::unique_ptr<DataType>& lhs, const std::unique_ptr<DataType>& rhs) {
    return static_cast<bool>(lhs) and static_cast<bool>(rhs) and *lhs == *rhs;
}

[[nodiscard]] const DataType* TypeContainer::from_type_definition(std::unique_ptr<DataType> type_definition) {
    const auto found = find(type_definition);
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

bool TypeContainer::is_defined(const std::unique_ptr<DataType>& data_type) const {
    using std::ranges::find_if;

    if (const auto pointer = dynamic_cast<const PointerType*>(data_type.get())) {
        return is_defined(pointer->contained->clone());
    }
    const auto concrete_type = dynamic_cast<const ConcreteType*>(data_type.get());
    assert(concrete_type and "not implemented");

    const auto found = find(data_type);
    return found != nullptr;
}

const DataType* TypeContainer::find(const std::unique_ptr<DataType>& data_type) const {
    using std::ranges::find_if;
    const auto find_iterator =
            find_if(m_data_types, [&](const auto& other) { return are_types_equal(data_type, other); });
    const auto data_type_found = (find_iterator != std::cend(m_data_types));
    return data_type_found ? find_iterator->get() : nullptr;
}

const DataType* TypeContainer::const_u32() const {
    return m_const_u32;
}

const DataType* TypeContainer::const_bool() const {
    return m_const_bool;
}

const DataType* TypeContainer::const_char() const {
    return m_const_char;
}

const DataType* TypeContainer::const_nothing() const {
    return m_const_nothing;
}
const DataType* TypeContainer::mutable_nothing() const {
    return m_mutable_nothing;
}
