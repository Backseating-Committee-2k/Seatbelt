//
// Created by coder2k on 15.10.2022.
//

#pragma once

#include "bssembly.hpp"
#include "location.hpp"
#include <filesystem>
#include <optional>
#include <string_view>
#include <upholsterer2k/parser.h>

namespace upholsterer2k {

    [[nodiscard]] Bssembler::Bssembly::InstructionVector
    parse_bssembly(const std::string_view filename, std::string_view source, Location origin_location);

    [[nodiscard]] bool
    write_machine_code_to_file(UP2K_ByteVector machine_code, const std::optional<std::filesystem::path>& path);

} // namespace upholsterer2k
