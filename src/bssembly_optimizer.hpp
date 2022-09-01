//
// Created by coder2k on 28.08.2022.
//

#pragma once

#include "bssembly.hpp"
#include <algorithm>
#include <array>
#include <fmt/core.h>
#include <memory>
#include <optional>
#include <ranges>
#include <unordered_map>
#include <vector>

namespace Optimizer {
    using namespace Bssembler;

    class Optimizer {
    public:
        virtual ~Optimizer() = default;

        [[nodiscard]] virtual usize window_size() const = 0;
        [[nodiscard]] virtual std::optional<std::vector<Bssembly::InstructionVariant>> optimize(
                std::span<Bssembly::InstructionVariant> instruction_variants
        ) const = 0;
    };

    class CopyPushPopOptimizer final : public Optimizer {
    public:
        CopyPushPopOptimizer() = default;

        [[nodiscard]] usize window_size() const override {
            return 3;
        }

        [[nodiscard]] std::optional<std::vector<Bssembly::InstructionVariant>> optimize(
                std::span<Bssembly::InstructionVariant> instruction_variants
        ) const override {
            using enum Mnemonic;
            const auto instructions = std::array{ std::get_if<Instruction>(&instruction_variants[0]),
                                                  std::get_if<Instruction>(&instruction_variants[1]),
                                                  std::get_if<Instruction>(&instruction_variants[2]) };
            if (not instructions[0] or not instructions[1] or not instructions[2]) {
                return {};
            }
            if (instructions[0]->mnemonic == COPY and instructions[1]->mnemonic == PUSH
                and instructions[2]->mnemonic == POP and instructions[2]->arguments.size() == 1) {
                const auto copy_target = instructions[0]->arguments.back();
                const auto push_source = instructions[1]->arguments.front();
                const auto pop_target = instructions[2]->arguments.back();
                if (copy_target == push_source) {
                    if (copy_target == pop_target) {
                        return std::vector<Bssembly::InstructionVariant>{
                            Instruction{COPY,
                                        { instructions[0]->arguments.front(), instructions[2]->arguments.front() },
                                        "CopyPushPopOptimizer"}
                        };
                    } else {
                        return std::vector<Bssembly::InstructionVariant>{
                            *instructions[0],
                            Instruction{COPY,
                                        { instructions[0]->arguments.front(), instructions[2]->arguments.front() },
                                        "CopyPushPopOptimizer"}
                        };
                    }
                }
            }
            return {};
        }
    };

    class PushImmediatePopOptimizer final : public Optimizer {
    public:
        PushImmediatePopOptimizer() = default;

        [[nodiscard]] usize window_size() const override {
            return 2;
        }

        [[nodiscard]] std::optional<std::vector<Bssembly::InstructionVariant>> optimize(
                std::span<Bssembly::InstructionVariant> instruction_variants
        ) const override {
            using enum Mnemonic;
            const auto instructions = std::array{ std::get_if<Instruction>(&instruction_variants[0]),
                                                  std::get_if<Instruction>(&instruction_variants[1]) };
            if (not instructions[0] or not instructions[1]) {
                return {};
            }
            if (instructions[0]->mnemonic == PUSH
                and std::holds_alternative<Immediate>(instructions[0]->arguments.front())
                and instructions[1]->mnemonic == POP and instructions[1]->arguments.size() == 1) {
                assert(instructions[0]->arguments.size() == 1);
                const auto immediate = instructions[0]->arguments[0];
                const auto pop_target = instructions[1]->arguments[0];
                return std::vector<Bssembly::InstructionVariant>{
                    Instruction{COPY, { immediate, pop_target }, "PushImmediatePopOptimizer"}
                };
            }
            return {};
        }
    };

    class UnnecessaryJumpOptimizer final : public Optimizer {
    public:
        UnnecessaryJumpOptimizer() = default;

        [[nodiscard]] usize window_size() const override {
            return 2;
        }

        [[nodiscard]] std::optional<std::vector<Bssembly::InstructionVariant>> optimize(
                std::span<Bssembly::InstructionVariant> instruction_variants
        ) const override {
            using enum Mnemonic;

            const auto instruction = std::get_if<Instruction>(&instruction_variants[0]);
            const auto label = std::get_if<Label>(&instruction_variants[1]);
            if (not instruction or not label or instruction->mnemonic != JUMP
                or not std::holds_alternative<Immediate>(instruction->arguments.front())) {
                return {};
            }
            const auto& jump_target = std::get<Immediate>(instruction->arguments.front());
            if (not std::holds_alternative<std::string>(jump_target.value)) {
                return {};
            }
            const auto& label_name = label->name;
            if (std::get<std::string>(jump_target.value) == label_name) {
                return std::vector<Bssembly::InstructionVariant>{
                    Label{label_name, "UnnecessaryJumpOptimizer"}
                };
            }
            return {};
        }
    };

    inline void strip_insignificant_lines(Bssembler::Bssembly& bssembly) {
        auto& instruction_vector = bssembly.instruction_vector();
        std::erase_if(instruction_vector, [](const auto& instruction) {
            return std::holds_alternative<Comment>(instruction) or std::holds_alternative<NewLine>(instruction);
        });
    }

    inline void collapse_labels(Bssembler::Bssembly& bssembly) {
        auto collapsed_labels = std::unordered_map<std::string, std::string>{};

        bool inside_label_list = false;
        usize list_start = 0;
        for (usize i = 0; i < bssembly.size(); ++i) {
            if (inside_label_list) {
                if (not std::holds_alternative<Label>(bssembly[i])) {
                    if (i > list_start + 1) {
                        const auto& collapsed_to = std::get<Label>(bssembly[list_start]).name;
                        for (usize j = list_start + 1; j < i; ++j) {
                            collapsed_labels[std::get<Label>(bssembly[j]).name] = collapsed_to;
                        }
                        bssembly.replace(list_start + 1, i, {});
                        i = list_start + 1;
                    }
                    inside_label_list = false;
                }
            } else {
                // not inside label list
                if (std::holds_alternative<Label>(bssembly[i])) {
                    list_start = i;
                    inside_label_list = true;
                }
            }
        }

        // replace all labels that have been collapsed
        for (usize i = 0; i < bssembly.size(); ++i) {
            if (const auto instruction = std::get_if<Instruction>(&bssembly[i])) {
                for (auto& argument : instruction->arguments) {
                    if (const auto immediate = std::get_if<Immediate>(&argument)) {
                        if (const auto string = std::get_if<std::string>(&immediate->value)) {
                            const auto find_iterator = collapsed_labels.find(*string);
                            const auto found = (find_iterator != collapsed_labels.end());
                            if (found) {
                                *string = find_iterator->second;
                            }
                        }
                    }
                }
            }
        }
    }
} // namespace Optimizer

inline void optimize(Bssembler::Bssembly& bssembly, const bool verbose) {
    using enum Bssembler::Mnemonic;
    using namespace Bssembler;

    Optimizer::strip_insignificant_lines(bssembly);
    Optimizer::collapse_labels(bssembly);

    auto optimizers = std::vector<std::unique_ptr<Optimizer::Optimizer>>{};
    optimizers.push_back(std::make_unique<Optimizer::CopyPushPopOptimizer>());
    optimizers.push_back(std::make_unique<Optimizer::PushImmediatePopOptimizer>());
    optimizers.push_back(std::make_unique<Optimizer::UnnecessaryJumpOptimizer>());

    bool did_optimize = true;
    usize pass = 1;
    for (; did_optimize; ++pass) {
        did_optimize = false;
        for (const auto& optimizer : optimizers) {
            const auto window_size = optimizer->window_size();

            for (usize i = 0; i < bssembly.size() - (window_size - 1); ++i) {
                auto instructions = std::span<Bssembly::InstructionVariant>{ bssembly.begin() + i, window_size };
                auto replacement = optimizer->optimize(instructions);
                if (replacement.has_value()) {
                    did_optimize = true;
                    bssembly.replace(i, i + window_size, std::span{ replacement->begin(), replacement->end() });
                }
            }
        }
    }
    if (verbose) {
        fmt::print(stderr, "ran {} optimizer passes\n", pass - 2);
    }
}
