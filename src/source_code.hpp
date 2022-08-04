//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include <string_view>

struct SourceCode {
    std::string_view filename;
    std::string_view text;
};
