//
// Created by coder2k on 30.08.2022.
//

#pragma once

#include <cassert>
#include <concepts>

namespace Utils {

    template<std::integral T>
    [[nodiscard]] inline T round_up(T num_to_round, T multiple) {
        // source: https://stackoverflow.com/a/3407254/7540548
        assert(multiple > 0);
        const T remainder = num_to_round % multiple;
        if (remainder == 0) {
            return num_to_round;
        }
        return num_to_round + multiple - remainder;
    }

    template<std::integral T>
    [[nodiscard]] inline T ceiling_division(T lhs, T rhs) {
        // source: https://stackoverflow.com/a/2745086/7540548

        // this implementation does not avoid overflow for huge values!
        return (lhs + rhs - 1) / rhs;
    }

}// namespace Utils
