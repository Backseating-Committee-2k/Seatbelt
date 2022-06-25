//
// Created by coder2k on 24.06.2022.
//

#include "error.hpp"
#include <ranges>
#include <algorithm>
#include <format>
#include <iostream>

[[nodiscard]] static Location token_location(const auto& token) {
    return std::visit([](const auto& token) { return token.location; }, token);
}

void Error::error(const Lexer::Tokens::Token& token, const std::string_view message) {
    using namespace std::ranges::views;
    using std::ranges::count, std::ranges::find;
    const auto npos = std::string_view::npos;

    const auto location = token_location(token);
    const auto row = count(location.source_code.text | take(location.offset_start_inclusive), '\n') + 1;
    const auto last_newline_pos = location.source_code.text.find_last_of('\n', location.offset_start_inclusive);
    const auto column = location.offset_start_inclusive - (last_newline_pos == npos ? -1 : last_newline_pos);

    const auto line_start_pos = last_newline_pos == npos ? 0 : last_newline_pos + 1;
    const auto next_newline_pos = location.source_code.text.find('\n', line_start_pos);
    const auto line_end_pos = next_newline_pos == npos ? location.source_code.text.length() : next_newline_pos;
    const auto line = location.source_code.text.substr(line_start_pos, line_end_pos - line_start_pos);

    std::cerr << std::format("{}:{}:{}: {}\n{}\n", location.source_code.filename, row, column, message, line);
    for (usize i = 0; i < column - 1; ++i) {
        std::cerr << ' ';
    }
    std::cerr << '^';
    const auto squiggly_length = std::max(location.view().length(), usize{ 1 }) - 1;
    for (usize i = 0; i < squiggly_length; ++i) {
        std::cerr << '~';
    }
    std::cerr << " error occurred here\n";
    std::exit(EXIT_FAILURE);
}
