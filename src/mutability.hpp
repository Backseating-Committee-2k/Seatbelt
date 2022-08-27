//
// Created by coder2k on 27.08.2022.
//

#pragma once

enum class Mutability {
    Mutable,
    Const,
};

[[nodiscard]] inline bool is_const(const Mutability mutability) {
    return mutability == Mutability::Const;
}

[[nodiscard]] inline bool is_mutable(const Mutability mutability) {
    return mutability == Mutability::Mutable;
}
