//
// Created by coder2k on 28.08.2022.
//

#pragma once

#include "bssembly.hpp"
#include <algorithm>
#include <array>
#include <fmt/core.h>
#include <ranges>

inline void optimize(Bssembler::Bssembly& bssembly) {
    using enum Bssembler::Mnemonic;
    using namespace Bssembler;

    static constexpr usize window_size = 3;
    for (usize i = 0; i < bssembly.size() - (window_size - 1); ++i) {
        auto instructions = std::array<Bssembler::Instruction*, window_size>{};
        bool all_are_instructions = true;
        for (usize j = 0; j < window_size; ++j) {
            instructions[j] = std::get_if<Bssembler::Instruction>(&bssembly[i + j]);
            if (instructions[j] == nullptr) {
                all_are_instructions = false;
                break;
            }
        }
        if (not all_are_instructions) {
            continue;
        }

        if (instructions[0]->mnemonic == COPY and instructions[1]->mnemonic == PUSH
            and instructions[2]->mnemonic == POP) {
            auto copy_target = instructions[0]->arguments.back();
            auto push_source = instructions[1]->arguments.front();
            if (copy_target == push_source) {
                bssembly.replace(
                        i, i + 3,
                        {
                                Instruction{
                                            COPY, { instructions[0]->arguments.front(), instructions[2]->arguments.front() }}
                }
                );
            }
        }
    }
}
