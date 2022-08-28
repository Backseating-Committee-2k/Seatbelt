//
// Created by coder2k on 28.08.2022.
//

#pragma once

#include "overloaded.hpp"
#include "types.hpp"
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace Bssembler {

    // clang-format off

#define MNEMONIC_LIST \
    x(COPY)           \
    x(ADD)            \
    x(SUB)            \
    x(MULT)           \
    x(DIVMOD)         \
    x(AND)                  \
    x(OR)                  \
    x(XOR)                  \
    x(COMP)                  \
    x(COMP_EQ)                  \
    x(COMP_NEQ)                  \
    x(COMP_GT)                  \
    x(COMP_GE)                  \
    x(COMP_LT)                  \
    x(COMP_LE)                  \
    x(JUMP)                  \
    x(JUMP_EQ)                  \
    x(JUMP_GT)                  \
    x(PUSH)           \
    x(POP) \
    x(RETURN) \
    x(HALT)

#define REGISTER_LIST \
    x(R0) \
    x(R1) \
    x(R2) \
    x(R3) \
    x(R4) \
    x(R5) \
    x(R6) \
    x(SP) \
    x(IP)

    // clang-format on

#define x(LIST_ENTRY) LIST_ENTRY,

    enum class Mnemonic { MNEMONIC_LIST };

    enum class Register { REGISTER_LIST };

#undef x

    struct DynamicRegister {
        u8 number;
    };

    [[nodiscard]] inline constexpr std::string_view mnemonic_to_string_view(Mnemonic mnemonic) {
#define x(LIST_ENTRY)          \
    case Mnemonic::LIST_ENTRY: \
        return #LIST_ENTRY;

        switch (mnemonic) { MNEMONIC_LIST }
#undef x
        assert(false and "unknown mnemonic");
        return "";
    }

    [[nodiscard]] inline constexpr std::string_view register_to_string_view(Register register_) {
#define x(LIST_ENTRY)          \
    case Register::LIST_ENTRY: \
        return #LIST_ENTRY;
        switch (register_) { REGISTER_LIST }
#undef x
        assert(false and "unknown register");
        return "";
    }

    struct NewLine { };

    struct Comment {
        std::string text;
    };

    struct InlineBssembly {
        std::string_view content;
    };

    struct Immediate {
        Immediate(std::string value) : value{ std::move(value) } { }

        Immediate(usize value) : value{ value } { }

        Immediate(std::string_view value) : value{ std::string{ value } } { }

        std::variant<std::string, usize> value;
    };

    struct Pointer {
        Register register_;
    };

    struct Label {
        Label(std::string name, std::string comment) : name{ std::move(name) }, comment{ std::move(comment) } { }

        explicit Label(std::string name) : name{ std::move(name) }, comment{} { }

        std::string name;
        std::optional<std::string> comment;
    };

    using InstructionArgument = std::variant<Register, DynamicRegister, Immediate, Pointer>;

    struct Instruction {
        Instruction(Mnemonic mnemonic, std::initializer_list<InstructionArgument> arguments, std::string comment)
            : mnemonic{ mnemonic },
              arguments{ arguments },
              comment{ comment } { }
        Instruction(Mnemonic mnemonic, std::initializer_list<InstructionArgument> arguments)
            : mnemonic{ mnemonic },
              arguments{ arguments },
              comment{} { }

        Mnemonic mnemonic;
        std::vector<InstructionArgument> arguments;
        std::optional<std::string> comment;

        [[nodiscard]] std::string to_string() const {
            using std::ranges::views::transform;
            auto result = std::string{ mnemonic_to_string_view(mnemonic) };
            if (not arguments.empty()) {
                result += " ";
            }
            result += fmt::format(
                    "{}",
                    fmt::join(
                            arguments | transform([](const auto& argument) { return argument_to_string(argument); }),
                            ", "
                    )
            );
            if (comment.has_value()) {
                result += fmt::format(" // {}", *comment);
            }
            return result;
        }

    private:
        [[nodiscard]] static std::string argument_to_string(const InstructionArgument& argument) {
            return std::visit(
                    overloaded{
                            [](const Register& register_) { return std::string{ register_to_string_view(register_) }; },
                            [](const DynamicRegister& register_) { return fmt::format("R{}", register_.number); },
                            [](const Immediate& immediate) {
                                return std::visit(
                                        overloaded{ [](const std::string& value) { return value; },
                                                    [](const usize value) { return std::to_string(value); } },
                                        immediate.value
                                );
                            },
                            [](const Pointer& pointer) {
                                return fmt::format("*{}", register_to_string_view(pointer.register_));
                            } },
                    argument
            );
        }
    };

    class Bssembly {
    public:
        Bssembly() = default;

        void add(auto&& instruction) {
            m_instructions.emplace_back(std::forward<decltype(instruction)>(instruction));
        }

        [[nodiscard]] std::string to_string() const {
            auto result = std::string{};
            for (const auto& instruction : m_instructions) {
                std::visit(
                        overloaded{ [&](const NewLine&) { result += '\n'; },
                                    [&](const Comment& comment) { result += fmt::format("\t// {}\n", comment.text); },
                                    [&](const Label& label) {
                                        result += fmt::format("{}:", label.name);
                                        if (label.comment.has_value()) {
                                            result += fmt::format(" // {}", *(label.comment));
                                        }
                                        result += '\n';
                                    },
                                    [&](const Instruction& instruction) {
                                        result += fmt::format("\t{}\n", instruction.to_string());
                                    },
                                    [&](const InlineBssembly& inline_bssembly) { result += inline_bssembly.content; } },
                        instruction
                );
            }
            return result;
        }

        [[nodiscard]] Bssembly operator+(const Bssembly& other) const {
            auto result = InstructionVector{ m_instructions };
            result.reserve(m_instructions.size() + other.m_instructions.size());
            for (const auto& instruction : other.m_instructions) {
                result.push_back(instruction);
            }
            return Bssembly{ result };
        }

        Bssembly& operator+=(const Bssembly& other) {
            m_instructions.reserve(m_instructions.size() + other.m_instructions.size());
            for (const auto& instruction : other.m_instructions) {
                m_instructions.push_back(instruction);
            }
            return *this;
        }

    private:
        using InstructionVector = std::vector<std::variant<NewLine, Comment, Instruction, Label, InlineBssembly>>;

    private:
        explicit Bssembly(InstructionVector instruction) : m_instructions{ std::move(instruction) } { }

    private:
        InstructionVector m_instructions;
    };
}// namespace Bssembler
