//
// Created by coder2k on 28.08.2022.
//

#pragma once

#include "data_type.hpp"
#include "overloaded.hpp"
#include "types.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <iterator>
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
    public:
        using InstructionVariant = std::variant<NewLine, Comment, Instruction, Label, InlineBssembly>;
        using InstructionVector = std::vector<InstructionVariant>;
        using iterator = InstructionVector::iterator;
        using const_iterator = InstructionVector::const_iterator;

    public:
        Bssembly() = default;

        void add(auto&& instruction) {
            m_instructions.emplace_back(std::forward<decltype(instruction)>(instruction));
        }

        void emit_mem_copy(
                const Register source_pointer,
                const Register destination_pointer,
                const usize size,
                const usize alignment
        ) {
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

            assert(alignment <= size);
            assert(size % alignment == 0);

            const auto num_copy_instructions = size / alignment;

            const auto instruction = offset_copy_instruction_from_size(alignment);
            for (usize i = 0; i < num_copy_instructions; ++i) {
                add(Instruction{
                        instruction,
                        {Pointer{ source_pointer }, Immediate{ i * alignment }, temp_register},
                        "fetch data"
                });
                add(Instruction{
                        instruction,
                        {temp_register, Immediate{ i * alignment }, Pointer{ destination_pointer }},
                        "write data"
                });
            }
        }

        void pop_from_stack_into_pointer(const Register pointer, const usize size) {
            using enum Register;
            using enum Mnemonic;

            assert(size % WordSize == 0);
            Register temp_register = (pointer == R1 ? R2 : R1);
            assert(temp_register != pointer);
            for (usize i = 0; i < size; i += WordSize) {
                const auto offset = size - i - WordSize;
                add(Instruction{ POP, { temp_register } });
                add(Instruction{
                        OFFSET_COPY,
                        {temp_register, Immediate{ offset }, Pointer{ pointer }}
                });
            }
        }

        /**
         * This function copies data from the stack (in expanded form) into an address given by a pointer (in non-
         * expanded form).
         * Example: When a array like [Bool; 5] lies on the stack (i.e. as a result of a function call), it occupies
         *          5 * WordSize = 20 bytes. When stored inside the stack frame of a function, it is stored in non-
         *          expanded form and only takes up 5 * type_size(Bool) = 5 bytes.
         * @param destination_pointer Register holding the target address.
         * @param stack_pointer Register holding the address where the stack data starts.
         * @param destination_offset Internal value used to control recursion.
         * @param stack_offset Internal value used to control recursion.
         */
        void copy_from_stack_into_pointer(
                const Register stack_pointer,
                const Register destination_pointer,
                const DataType* data_type,
                const usize stack_offset = 0,
                const usize destination_offset = 0
        ) {
            using enum Register;
            using enum Mnemonic;

            Register temp_register = R0;
            for (int i = static_cast<int>(R1); i < static_cast<int>(R253); ++i) {
                const auto current = static_cast<Register>(i);
                if (current != stack_pointer and current != destination_pointer) {
                    temp_register = current;
                    break;
                }
            }
            assert(temp_register != stack_pointer and temp_register != destination_pointer);

            if (data_type->is_primitive_type() or data_type->is_pointer_type()
                or data_type->is_function_pointer_type()) {
                assert(data_type->size() <= WordSize);
                if (data_type->size() > 0) {
                    add(Instruction{
                            OFFSET_COPY,
                            {Pointer{ stack_pointer }, Immediate{ stack_offset }, temp_register}
                    });
                    const auto instruction = offset_copy_instruction_from_size(data_type->size());
                    add(Instruction{
                            instruction,
                            {temp_register, Immediate{ destination_offset }, Pointer{ destination_pointer }}
                    });
                }
            } else if (data_type->is_array_type()) {
                const auto array_type = *(data_type->as_array_type());
                const auto size = array_type->contained->size();
                const auto size_when_pushed = array_type->contained->size_when_pushed();
                for (usize i = 0; i < array_type->num_elements; ++i) {
                    copy_from_stack_into_pointer(
                            stack_pointer, destination_pointer, array_type->contained,
                            stack_offset + i * size_when_pushed, destination_offset + i * size
                    );
                }
            } else {
                assert(false and "not implemented");
            }
        }

        void pop_from_stack_into_pointer(const Register pointer, const DataType* data_type, const usize offset = 0) {
            using enum Register;
            using enum Mnemonic;

            Register temp_register = (pointer == R1 ? R2 : R1);
            assert(temp_register != pointer);

            if (data_type->is_primitive_type() or data_type->is_pointer_type()
                or data_type->is_function_pointer_type()) {
                assert(data_type->size() <= WordSize);
                if (data_type->size() > 0) {
                    add(Instruction{ POP, { temp_register } });
                    const auto instruction = offset_copy_instruction_from_size(data_type->size());
                    add(Instruction{
                            instruction,
                            {temp_register, Immediate{ offset }, Pointer{ pointer }}
                    });
                }
            } else if (data_type->is_array_type()) {
                const auto array_type = *(data_type->as_array_type());
                for (usize i = 0; i < array_type->num_elements; ++i) {
                    const usize index = array_type->num_elements - i - 1;
                    const auto new_offset = offset + index * array_type->contained->size();
                    pop_from_stack_into_pointer(pointer, array_type->contained, new_offset);
                }
            } else if (data_type->is_struct_type()) {
                const auto struct_type = *(data_type->as_struct_type());
                const auto num_members = struct_type->members.size();

                auto offsets = std::vector<usize>{};
                offsets.reserve(num_members);
                usize current_offset = offset;
                for (const auto& member : struct_type->members) {
                    current_offset = Utils::round_up(current_offset, member.data_type->alignment());
                    offsets.push_back(current_offset);
                    current_offset += member.data_type->size();
                }

                for (usize i = 0; i < num_members; ++i) {
                    const usize index = num_members - i - 1;
                    pop_from_stack_into_pointer(pointer, struct_type->members[index].data_type, offsets[index]);
                }
            } else {
                assert(false and "not implemented");
            }
        }

        void push_value_onto_stack(const Register source_pointer, const DataType* data_type, const usize offset = 0) {
            using enum Register;
            using enum Mnemonic;

            const auto temp_register = (source_pointer == R1 ? R2 : R1);

            if (data_type->is_primitive_type() or data_type->is_pointer_type()
                or data_type->is_function_pointer_type()) {
                assert(data_type->size() <= WordSize);
                if (data_type->size() > 0) {
                    const auto instruction = offset_copy_instruction_from_size(data_type->size());
                    add(Instruction{
                            instruction,
                            {Pointer{ source_pointer }, Immediate{ offset }, temp_register}
                    });
                    add(Instruction{ PUSH, { temp_register } });
                }
            } else if (data_type->is_array_type()) {
                const auto array_type = *(data_type->as_array_type());
                const auto size = array_type->contained->size();
                for (usize i = 0; i < array_type->num_elements; ++i) {
                    push_value_onto_stack(source_pointer, array_type->contained, offset + i * size);
                }
            } else if (data_type->is_struct_type()) {
                const auto struct_type = *(data_type->as_struct_type());
                usize current_offset = offset;
                for (const auto& attribute : struct_type->members) {
                    current_offset = Utils::round_up(current_offset, attribute.data_type->alignment());
                    push_value_onto_stack(source_pointer, attribute.data_type, current_offset);
                    current_offset += attribute.data_type->size();
                }
            } else {
                assert(false and "not implemented");
            }
        }

        /**
         * Pushes a value from the stack (or from a memory region that was part of the stack before) onto the stack.
         * The address referenced by stack_pointer must point to a value in its expanded form (pushed form).
         * @param stack_pointer source pointer
         * @param data_type data type of the value to push
         * @param offset internal offset used for recursion
         */
        void push_onto_stack_from_stack_pointer(
                const Register stack_pointer,
                const DataType* data_type,
                const usize offset = 0
        ) {
            using enum Mnemonic;
            using enum Register;

            const auto temp_register = (stack_pointer == R1 ? R2 : R1);

            assert(offset % WordSize == 0);

            if (data_type->is_primitive_type() or data_type->is_pointer_type()
                or data_type->is_function_pointer_type()) {
                assert(data_type->size_when_pushed() == 0 or data_type->size_when_pushed() == WordSize);
                if (data_type->size_when_pushed() == WordSize) {
                    add(Instruction{
                            OFFSET_COPY,
                            {Pointer{ stack_pointer }, Immediate{ offset }, temp_register}
                    });
                    add(Instruction{ PUSH, { temp_register } });
                }
            } else if (data_type->is_array_type()) {
                const auto array_type = *(data_type->as_array_type());
                usize current_offset = offset;
                for (usize i = 0; i < array_type->num_elements; ++i) {
                    push_onto_stack_from_stack_pointer(stack_pointer, array_type->contained, current_offset);
                    current_offset += array_type->contained->size_when_pushed();
                }
            } else if (data_type->is_struct_type()) {
                const auto struct_type = *(data_type->as_struct_type());
                usize current_offset = offset;
                for (const auto& attribute : struct_type->members) {
                    push_onto_stack_from_stack_pointer(stack_pointer, attribute.data_type, current_offset);
                    current_offset += attribute.data_type->size_when_pushed();
                }
            } else {
                assert(false and "not implemented");
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

        Bssembly& operator+=(Bssembly&& other) {
            std::ranges::move(other.m_instructions, std::back_inserter(m_instructions));
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

        void replace(usize from, usize to, std::span<InstructionVariant> replacement) {
            // TODO: this is REALLY inefficient and should be optimized!
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

        [[nodiscard]] InstructionVector& instruction_vector() {
            return m_instructions;
        }

    private:
        [[nodiscard]] Mnemonic offset_copy_instruction_from_size(const usize size) {
            using enum Mnemonic;
            switch (size) {
                case WordSize:
                    return OFFSET_COPY;
                case HalfwordSize:
                    return OFFSET_COPY_HALFWORD;
                case 1:
                    return OFFSET_COPY_BYTE;
                default:
                    assert(false and "unreachable");
                    return OFFSET_COPY;
            }
        }

    private:
        explicit Bssembly(InstructionVector instruction) : m_instructions{ std::move(instruction) } { }

    private:
        InstructionVector m_instructions;
    };
} // namespace Bssembler
