//
// Created by micha on 24.10.2022.
//

#include "data_type.hpp"
#include "utils.hpp"

[[nodiscard]] usize StructType::size() const {
    if (owning_custom_type != nullptr) {
        return owning_custom_type->size();
    }
    // this is a struct type that stems from an anonymous custom type => is has no tag
    usize result = 0;
    for (const auto& member : members) {
        result = Utils::round_up(result, member.data_type->alignment());
        result += member.data_type->size();
    }
    // all structs have to have a size that's a multiple of their alignment and all structs must have an
    // alignment of 4 bytes (WordSize)
    result = Utils::round_up(result, WordSize);
    return result;
}
