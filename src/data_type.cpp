//
// Created by micha on 24.10.2022.
//

#include "data_type.hpp"
#include "parser.hpp"
#include "utils.hpp"

[[nodiscard]] usize StructType::size() const {
    if (owning_custom_type_definition != nullptr) {
        return owning_custom_type_definition->data_type->size();
    }
    // this is a struct type that stems from an anonymous custom type => is has no tag
    usize result = 0;
    for (const auto& member : members) {
        result = Utils::round_up(result, member.data_type->alignment());
        result += member.data_type->size();
    }
    // all structs have to have a size that's a multiple of their alignment
    return Utils::round_up(result, alignment());
}

[[nodiscard]] usize StructType::size_when_pushed() const {
    if (owning_custom_type_definition != nullptr) {
        return owning_custom_type_definition->data_type->size_when_pushed();
    }
    return size_when_pushed_without_padding();
}

[[nodiscard]] bool StructType::contains_tag() const {
    if (owning_custom_type_definition == nullptr) {
        return false;
    }
    const auto owning_type = *(owning_custom_type_definition->data_type->as_custom_type());
    return owning_type->contains_tag();
}

[[nodiscard]] std::optional<u32> StructType::tag() const {
    if (not contains_tag()) {
        return {};
    }
    assert(owning_custom_type_definition != nullptr);
    const auto owning_type = *(owning_custom_type_definition->data_type->as_custom_type());
    for (const auto& [tag, struct_type] : owning_type->struct_types) {
        if (struct_type == this) {
            return tag;
        }
    }
    assert(false and "unreachable");
    return {};
}

[[nodiscard]] bool StructType::contains_placeholders() const {
    for (const auto& member : members) {
        if (member.data_type->contains_placeholders()) {
            return true;
        }
    }
    if (owning_custom_type_definition == nullptr) {
        return false;
    }
    return owning_custom_type_definition->data_type->is_custom_type_placeholder();
}

[[nodiscard]] usize StructType::alignment() const {
    if (contains_tag()) {
        return WordSize;
    }
    const auto max_alignment_iterator =
            std::max_element(members.cbegin(), members.cend(), [](const auto& lhs, const auto& rhs) {
                return lhs.data_type->alignment() < rhs.data_type->alignment();
            });
    const auto found = (max_alignment_iterator != members.cend());
    if (not found) {
        return 1;
    }
    const auto result = (*max_alignment_iterator).data_type->alignment();
    return result;
}
