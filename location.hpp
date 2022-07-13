//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include "source_code.hpp"
#include "types.hpp"
#include <string_view>

struct Location {
    SourceCode source_code;
    usize offset_start_inclusive;
    usize offset_end_exclusive;

    [[nodiscard]] std::string_view view() const {
        return source_code.text.substr(offset_start_inclusive, offset_end_exclusive - offset_start_inclusive);
    }
};
