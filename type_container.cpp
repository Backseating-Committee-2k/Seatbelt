//
// Created by coder2k on 13.07.2022.
//

#include "type_container.hpp"
#include "data_type.hpp"
#include "error.hpp"
#include <algorithm>
#include <ranges>

[[nodiscard]] static bool are_types_equal(const std::unique_ptr<DataType>& lhs, const std::unique_ptr<DataType>& rhs) {
    return static_cast<bool>(lhs) and static_cast<bool>(rhs) and *lhs == *rhs;
}

[[nodiscard]] const DataType* TypeContainer::from_type_definition(std::unique_ptr<DataType> type_definition) {
    using std::ranges::find_if;
    const auto find_iterator =
            find_if(m_data_types, [&](const auto& other) { return are_types_equal(type_definition, other); });
    const auto data_type_found = (find_iterator != std::cend(m_data_types));
    if (data_type_found) {
        return find_iterator->get();
    } else {
        m_data_types.push_back(std::move(type_definition));
        return m_data_types.back().get();
    }
}
