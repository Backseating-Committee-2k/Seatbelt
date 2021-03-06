//
// Created by coder2k on 24.06.2022.
//

#include "error.hpp"
#include <algorithm>
#include <fmt/core.h>
#include <iostream>
#include <ranges>
#include <stdexcept>

void print_message(const Lexer::Tokens::Token& token, const std::string_view message) {
    using namespace std::ranges::views;
    using std::ranges::count, std::ranges::find;
    const auto npos = std::string_view::npos;

    const auto location = Error::token_location(token);
    const auto row = count(location.source_code.text | take(location.offset_start_inclusive), '\n') + 1;
    const auto last_newline_pos = location.source_code.text.find_last_of('\n', location.offset_start_inclusive);
    const auto column = location.offset_start_inclusive - (last_newline_pos == npos ? -1 : last_newline_pos);

    const auto line_start_pos = last_newline_pos == npos ? 0 : last_newline_pos + 1;
    const auto next_newline_pos = location.source_code.text.find('\n', line_start_pos);
    const auto line_end_pos = next_newline_pos == npos ? location.source_code.text.length() : next_newline_pos;
    const auto line = location.source_code.text.substr(line_start_pos, line_end_pos - line_start_pos);

    std::cerr << fmt::format("{}:{}:{}: {}\n{}\n", location.source_code.filename, row, column, message, line);
    for (usize i = 0; i < column - 1; ++i) {
        std::cerr << ' ';
    }
    std::cerr << '^';
    const auto squiggly_length = std::min(std::max(location.view().length(), usize{ 1 }) - 1, line.length() - column);
    for (usize i = 0; i < squiggly_length; ++i) {
        std::cerr << '~';
    }
}

void Error::error(const Lexer::Tokens::Token& token, const std::string_view message) {
    print_message(token, message);
    std::cerr << " error occurred here\n";
    // throw std::runtime_error{ std::string{ message } };
    std::exit(EXIT_FAILURE);
}

void Error::warning(const Lexer::Tokens::Token& token, const std::string_view message) {
    print_message(token, fmt::format("warning: {}", message));
    std::cerr << " see here\n";
}
