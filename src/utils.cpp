//
// Created by coder2k on 31.08.2022.
//

#include "utils.hpp"
#include <algorithm>
#include <array>
#include <cctype>
#include <ranges>

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

    [[nodiscard]] std::string_view trim(std::string_view view) {
        using std::ranges::find_if, std::ranges::views::reverse;
        const auto left_find_iterator = find_if(view, [](char c) { return not std::isspace(c); });
        const auto found = (left_find_iterator != view.cend());
        if (not found) {
            return std::string_view{ "" };
        }
        const auto right_find_iterator = find_if(view | reverse, [](char c) { return not std::isspace(c); });
        const auto left_index = left_find_iterator - view.cbegin();
        const auto right_index = right_find_iterator.base() - view.cbegin();
        const auto length = right_index - left_index;
        view = view.substr(left_index, length);
        return view;
    }

    [[nodiscard]] std::string to_upper(std::string_view view) {
        auto string = std::string{ view };
        for (auto& c : string) {
            c = static_cast<char>(std::toupper(c));
        }
        return string;
    }

} // namespace Utils
