//
// Created by coder2k on 31.08.2022.
//

#include "utils.hpp"
#include <array>
#include <cctype>

namespace Utils {

    [[nodiscard]] u8 char_to_digit(char c, const u64 base) {
        constexpr auto valid_digits =
                std::array{ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };
        assert(base <= 16);
        c = static_cast<char>(std::toupper(c));
        for (usize i = 0; i < base; ++i) {
            if (c == valid_digits[i]) {
                if (i < 10) {
                    return static_cast<u8>(c - '0');
                } else {
                    return static_cast<u8>(c - 'A' + 10);
                }
            }
        }
        assert(false and "invalid input character");
        return 0;
    }

    [[nodiscard]] std::string strip_underscores(const std::string_view text) {
        auto result = std::string{};
        for (const auto c : text) {
            if (c != '_') {
                result += c;
            }
        }
        return result;
    }

    [[nodiscard]] usize get_base(std::string_view number_string) {
        if (number_string.starts_with("0x")) {
            return 16;
        } else if (number_string.starts_with("0o")) {
            return 8;
        } else if (number_string.starts_with("0b")) {
            return 2;
        }
        return 10;
    }

} // namespace Utils
