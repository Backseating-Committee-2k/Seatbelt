//
// Created by coder2k on 15.10.2022.
//

#pragma once

#include "bssembly.hpp"
#include "location.hpp"
#include <string_view>

[[nodiscard]] Bssembler::Bssembly::InstructionVector
parse_bssembly(const std::string_view filename, std::string_view source, Location origin_location);
