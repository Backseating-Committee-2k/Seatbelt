//
// Created by coder2k on 30.08.2022.
//

#pragma once

#include "types.hpp"
#include <cassert>
#include <concepts>
#include <ranges>
#include <string>
#include <string_view>

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

    [[nodiscard]] u8 char_to_digit(char c, u64 base);

    /**
     * This function checks if the given string view represents an unsigned number in the given base. The
     * base-prefix (e.g. 0x) must have been stripped before calling the function. Also all underscores
     * have to be stripped before.
     * @tparam T The data type whose size is considered for the maximum allowed value.
     * @param text The string representation of the number.
     * @param base The base.
     * @return True if the number is valid, false otherwise.
     */
    template<std::integral T>
    [[nodiscard]] inline bool validate_integer(const std::string_view text, const u64 base) {
        static_assert(sizeof(T) < sizeof(u64));
        u64 result = 0;
        u64 factor = 1;
        for (char c : std::ranges::reverse_view(text)) {
            const auto digit = char_to_digit(c, base);
            result += digit * factor;
            factor *= base;
            if (result > std::numeric_limits<T>::max()) {
                return false;
            }
        }
        return true;
    }

    [[nodiscard]] std::string strip_underscores(std::string_view text);

    [[nodiscard]] usize get_base(std::string_view number_string);

    template<std::integral T>
    [[nodiscard]] inline T parse_number(const std::string_view number_string, const u64 base) {
        using std::ranges::reverse_view;
        T factor = 1;
        T result = 0;
        for (auto c : reverse_view(number_string)) {
            const auto digit = static_cast<T>(char_to_digit(c, base));
            result += factor * digit;
            factor *= static_cast<T>(base);
        }
        return result;
    }

} // namespace Utils
