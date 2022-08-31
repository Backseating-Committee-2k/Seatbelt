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

#define MNEMONIC_LIST       \
    x(COPY)                 \
    x(COPY_BYTE)            \
    x(COPY_HALFWORD)        \
    x(OFFSET_COPY)          \
    x(OFFSET_COPY_BYTE)     \
    x(OFFSET_COPY_HALFWORD) \
    x(ADD)                  \
    x(SUB)                  \
    x(MULT)                 \
    x(DIVMOD)               \
    x(AND)                  \
    x(OR)                   \
    x(XOR)                  \
    x(LSHIFT)               \
    x(RSHIFT)               \
    x(COMP)                 \
    x(COMP_EQ)              \
    x(COMP_NEQ)             \
    x(COMP_GT)              \
    x(COMP_GE)              \
    x(COMP_LT)              \
    x(COMP_LE)              \
    x(JUMP)                 \
    x(JUMP_EQ)              \
    x(JUMP_GT)              \
    x(PUSH)                 \
    x(POP)                  \
    x(RETURN)               \
    x(HALT)

#define REGISTER_LIST \
    x(R0) \
    x(R1) \
    x(R2) \
    x(R3) \
    x(R4) \
    x(R5) \
    x(R6) \
    x(R7) \
    x(R8) \
    x(R9) \
    x(R10) \
    x(R11) \
    x(R12) \
    x(R13) \
    x(R14) \
    x(R15) \
    x(R16) \
    x(R17) \
    x(R18) \
    x(R19) \
    x(R20) \
    x(R21) \
    x(R22) \
    x(R23) \
    x(R24) \
    x(R25) \
    x(R26) \
    x(R27) \
    x(R28) \
    x(R29) \
    x(R30) \
    x(R31) \
    x(R32) \
    x(R33) \
    x(R34) \
    x(R35) \
    x(R36) \
    x(R37) \
    x(R38) \
    x(R39) \
    x(R40) \
    x(R41) \
    x(R42) \
    x(R43) \
    x(R44) \
    x(R45) \
    x(R46) \
    x(R47) \
    x(R48) \
    x(R49) \
    x(R50) \
    x(R51) \
    x(R52) \
    x(R53) \
    x(R54) \
    x(R55) \
    x(R56) \
    x(R57) \
    x(R58) \
    x(R59) \
    x(R60) \
    x(R61) \
    x(R62) \
    x(R63) \
    x(R64) \
    x(R65) \
    x(R66) \
    x(R67) \
    x(R68) \
    x(R69) \
    x(R70) \
    x(R71) \
    x(R72) \
    x(R73) \
    x(R74) \
    x(R75) \
    x(R76) \
    x(R77) \
    x(R78) \
    x(R79) \
    x(R80) \
    x(R81) \
    x(R82) \
    x(R83) \
    x(R84) \
    x(R85) \
    x(R86) \
    x(R87) \
    x(R88) \
    x(R89) \
    x(R90) \
    x(R91) \
    x(R92) \
    x(R93) \
    x(R94) \
    x(R95) \
    x(R96) \
    x(R97) \
    x(R98) \
    x(R99) \
    x(R100) \
    x(R101) \
    x(R102) \
    x(R103) \
    x(R104) \
    x(R105) \
    x(R106) \
    x(R107) \
    x(R108) \
    x(R109) \
    x(R110) \
    x(R111) \
    x(R112) \
    x(R113) \
    x(R114) \
    x(R115) \
    x(R116) \
    x(R117) \
    x(R118) \
    x(R119) \
    x(R120) \
    x(R121) \
    x(R122) \
    x(R123) \
    x(R124) \
    x(R125) \
    x(R126) \
    x(R127) \
    x(R128) \
    x(R129) \
    x(R130) \
    x(R131) \
    x(R132) \
    x(R133) \
    x(R134) \
    x(R135) \
    x(R136) \
    x(R137) \
    x(R138) \
    x(R139) \
    x(R140) \
    x(R141) \
    x(R142) \
    x(R143) \
    x(R144) \
    x(R145) \
    x(R146) \
    x(R147) \
    x(R148) \
    x(R149) \
    x(R150) \
    x(R151) \
    x(R152) \
    x(R153) \
    x(R154) \
    x(R155) \
    x(R156) \
    x(R157) \
    x(R158) \
    x(R159) \
    x(R160) \
    x(R161) \
    x(R162) \
    x(R163) \
    x(R164) \
    x(R165) \
    x(R166) \
    x(R167) \
    x(R168) \
    x(R169) \
    x(R170) \
    x(R171) \
    x(R172) \
    x(R173) \
    x(R174) \
    x(R175) \
    x(R176) \
    x(R177) \
    x(R178) \
    x(R179) \
    x(R180) \
    x(R181) \
    x(R182) \
    x(R183) \
    x(R184) \
    x(R185) \
    x(R186) \
    x(R187) \
    x(R188) \
    x(R189) \
    x(R190) \
    x(R191) \
    x(R192) \
    x(R193) \
    x(R194) \
    x(R195) \
    x(R196) \
    x(R197) \
    x(R198) \
    x(R199) \
    x(R200) \
    x(R201) \
    x(R202) \
    x(R203) \
    x(R204) \
    x(R205) \
    x(R206) \
    x(R207) \
    x(R208) \
    x(R209) \
    x(R210) \
    x(R211) \
    x(R212) \
    x(R213) \
    x(R214) \
    x(R215) \
    x(R216) \
    x(R217) \
    x(R218) \
    x(R219) \
    x(R220) \
    x(R221) \
    x(R222) \
    x(R223) \
    x(R224) \
    x(R225) \
    x(R226) \
    x(R227) \
    x(R228) \
    x(R229) \
    x(R230) \
    x(R231) \
    x(R232) \
    x(R233) \
    x(R234) \
    x(R235) \
    x(R236) \
    x(R237) \
    x(R238) \
    x(R239) \
    x(R240) \
    x(R241) \
    x(R242) \
    x(R243) \
    x(R244) \
    x(R245) \
    x(R246) \
    x(R247) \
    x(R248) \
    x(R249) \
    x(R250) \
    x(R251) \
    x(R252) \
    x(R253) \
    x(R254) \
    x(R255) \
    x(SP) \
    x(IP)

    // clang-format on

#define x(LIST_ENTRY) LIST_ENTRY,

    enum class Mnemonic { MNEMONIC_LIST };

    enum class Register { REGISTER_LIST };

#undef x

    struct DynamicRegister {
        [[nodiscard]] bool operator==(const DynamicRegister& other) const = default;

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

        [[nodiscard]] bool operator==(const Immediate& other) const = default;

        std::variant<std::string, usize> value;
    };

    struct Pointer {
        [[nodiscard]] bool operator==(const Pointer& other) const = default;

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
    private:
        using InstructionVariant = std::variant<NewLine, Comment, Instruction, Label, InlineBssembly>;
        using InstructionVector = std::vector<InstructionVariant>;

    public:
        using iterator = InstructionVector::iterator;
        using const_iterator = InstructionVector::const_iterator;

    public:
        Bssembly() = default;

        void add(auto&& instruction) {
            m_instructions.emplace_back(std::forward<decltype(instruction)>(instruction));
        }

        void emit_mem_copy(const Register source_pointer, const Register destination_pointer, usize size) {
            if (size == 0 or source_pointer == destination_pointer) {
                return;
            }
            using enum Register;
            using enum Mnemonic;

            Register temp_register = R0;
            for (int i = static_cast<int>(R1); i < static_cast<int>(R253); ++i) {
                const auto current = static_cast<Register>(i);
                if (current != source_pointer and current != destination_pointer) {
                    temp_register = current;
                    break;
                }
            }
            assert(temp_register != source_pointer and temp_register != destination_pointer);

            Register source_address = R0;
            for (int i = static_cast<int>(R1); i < static_cast<int>(R253); ++i) {
                const auto current = static_cast<Register>(i);
                if (current != source_pointer and current != destination_pointer and current != temp_register) {
                    source_address = current;
                    break;
                }
            }
            assert(source_address != source_pointer and source_address != destination_pointer
                   and source_address != temp_register);

            add(Instruction{
                    COPY,
                    {source_pointer, source_address}
            });
            // TODO: optimize by utilizing COPY and COPY_HALFWORD instead of always using COPY_BYTE
            for (usize i = 0; i < size; ++i) {
                add(Instruction{
                        COPY_BYTE,
                        {Pointer{ source_address }, temp_register},
                        "fetch byte"
                });
                add(Instruction{
                        OFFSET_COPY_BYTE,
                        {temp_register, Immediate{ i }, Pointer{ destination_pointer }},
                        "copy byte"
                });
                if (i < size - 1) {
                    add(Instruction{
                            ADD,
                            {source_address, 1, source_address},
                            "increase source pointer"
                    });
                }
            }
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

        [[nodiscard]] usize size() const {
            return m_instructions.size();
        }

        [[nodiscard]] iterator begin() {
            return m_instructions.begin();
        }

        [[nodiscard]] const_iterator begin() const {
            return m_instructions.begin();
        }

        [[nodiscard]] iterator end() {
            return m_instructions.begin();
        }

        [[nodiscard]] const_iterator end() const {
            return m_instructions.begin();
        }

        [[nodiscard]] InstructionVariant& operator[](usize index) {
            return m_instructions[index];
        }

        [[nodiscard]] const InstructionVariant& operator[](usize index) const {
            return m_instructions[index];
        }

        void replace(usize from, usize to, std::initializer_list<InstructionVariant> replacement) {
            m_instructions.erase(
                    m_instructions.begin() + static_cast<decltype(m_instructions)::difference_type>(from),
                    m_instructions.begin() + static_cast<decltype(m_instructions)::difference_type>(to)
            );
            auto position = static_cast<decltype(m_instructions)::difference_type>(from);
            for (const auto& instruction : replacement) {
                m_instructions.insert(m_instructions.begin() + position, instruction);
                ++position;
            }
        }

    private:
        explicit Bssembly(InstructionVector instruction) : m_instructions{ std::move(instruction) } { }

    private:
        InstructionVector m_instructions;
    };
} // namespace Bssembler
