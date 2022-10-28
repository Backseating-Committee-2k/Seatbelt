//
// Created by coder2k on 16.10.2022.
//

#include "bssembly.hpp"
#include "emitter.hpp"
#include <limits>

namespace Bssembler {

    void
    Bssembly::pop_from_stack_into_pointer(const Register pointer, const usize size, const Location origin_location) {
        using enum Register;
        using enum Mnemonic;

        assert(size % WordSize == 0);
        Register temp_register = (pointer == R1 ? R2 : R1);
        assert(temp_register != pointer);
        for (usize i = 0; i < size; i += WordSize) {
            const auto offset = size - i - WordSize;
            add(Instruction{
                    POP,
                    { temp_register },
                    origin_location,
            });
            add(Instruction{
                    OFFSET_COPY,
                    {temp_register, Immediate{ offset }, Pointer{ pointer }},
                    origin_location,
            });
        }
    }

    void Bssembly::copy_from_stack_into_pointer(
            const Register stack_pointer,
            const Register destination_pointer,
            const DataType* data_type,
            const Location origin_location,
            const usize stack_offset,
            const usize destination_offset
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

        if (data_type->is_primitive_type() or data_type->is_pointer_type() or data_type->is_function_pointer_type()) {
            assert(data_type->size() <= WordSize);
            if (data_type->size() > 0) {
                add(Instruction{
                        OFFSET_COPY,
                        {Pointer{ stack_pointer }, Immediate{ stack_offset }, temp_register},
                        origin_location,
                });
                const auto instruction = offset_copy_instruction_from_size(data_type->size());
                add(Instruction{
                        instruction,
                        {temp_register, Immediate{ destination_offset }, Pointer{ destination_pointer }},
                        origin_location,
                });
            }
        } else if (data_type->is_array_type()) {
            const auto array_type = *(data_type->as_array_type());
            const auto size = array_type->contained->size();
            const auto size_when_pushed = array_type->contained->size_when_pushed();
            for (usize i = 0; i < array_type->num_elements; ++i) {
                copy_from_stack_into_pointer(
                        stack_pointer, destination_pointer, array_type->contained, origin_location,
                        stack_offset + i * size_when_pushed, destination_offset + i * size
                );
            }
        } else {
            assert(false and "not implemented");
        }
    }

    template<std::size_t count>
    [[nodiscard]] std::array<Register, count> get_temp_registers(Register reserved) {
        using enum Register;
        auto result = std::array<Register, count>{};
        usize next_to_set = 0;
        Register current = R1; // R0 is reserved for the stack frame base pointer
        while (true) {
            if (next_to_set >= count) {
                break;
            }
            if (current != reserved) {
                result[next_to_set] = current;
                ++next_to_set;
            }
            current = static_cast<Register>(static_cast<int>(current) + 1);
        }
        return result;
    }

    void Bssembly::pop_from_stack_into_pointer(
            const Register pointer,
            const DataType* data_type,
            const Location origin_location,
            Emitter::LabelGenerator& label_generator,
            const usize offset
    ) {
        using enum Register;
        using enum Mnemonic;

        const auto temp_registers = get_temp_registers<3>(pointer);

        if (data_type->is_primitive_type() or data_type->is_pointer_type() or data_type->is_function_pointer_type()) {
            assert(data_type->size() <= WordSize);
            if (data_type->size() > 0) {
                add(Instruction{
                        POP,
                        { temp_registers[0] },
                        origin_location,
                });
                const auto instruction = offset_copy_instruction_from_size(data_type->size());
                add(Instruction{
                        instruction,
                        {
                          temp_registers[0],
                          Immediate{ offset },
                          Pointer{ pointer },
                          },
                        origin_location,
                });
            }
        } else if (data_type->is_array_type()) {
            const auto array_type = *(data_type->as_array_type());
            for (usize i = 0; i < array_type->num_elements; ++i) {
                const usize index = array_type->num_elements - i - 1;
                const auto new_offset = offset + index * array_type->contained->size();
                pop_from_stack_into_pointer(
                        pointer, array_type->contained, origin_location, label_generator, new_offset
                );
            }
        } else if (data_type->is_struct_type()) {
            const auto struct_type = *(data_type->as_struct_type());
            pop_struct_from_stack_into_pointer(
                    pointer, struct_type, temp_registers[0], origin_location, label_generator, offset
            );
        } else if (data_type->is_custom_type()) {
            const auto custom_type = *(data_type->as_custom_type());
            /* If the custom type has no tag, it only has one struct variant, and we can treat it as if it was the
             * actual struct.
             * Otherwise, we have to branch at runtime depending on the actual tag. */
            if (not custom_type->contains_tag()) {
                assert(custom_type->struct_types.size() == 1);
                const auto struct_type = custom_type->struct_types.begin()->second;
                pop_struct_from_stack_into_pointer(
                        pointer, struct_type, temp_registers[0], origin_location, label_generator, offset
                );
            } else {
                // we have to inspect the current set tag

                // for every variant we need a label pointing to the code to execute
                auto labels = std::vector<std::string>{};
                labels.reserve(custom_type->struct_types.size());
                for (const auto& pair : custom_type->struct_types) {
                    labels.push_back(label_generator.next_label(fmt::format("custom_type_tag_{}", pair.first)));
                }

                const auto end_label = label_generator.next_label("custom_type_end");

                // first we calculate the offset of the tag relative to the stack pointer (intentional wrap-around)
                const auto tag_offset = static_cast<u32>(-static_cast<i64>(custom_type->size_when_pushed()));
                add(Instruction{
                        OFFSET_COPY,
                        {Pointer{ SP }, Immediate{ tag_offset }, temp_registers[0]},
                        "get the actual value of the type tag"
                });

                for (usize i = 0; const auto& [tag, struct_type] : custom_type->struct_types) {
                    add(Instruction{
                            COPY,
                            {Immediate{ tag }, temp_registers[1]},
                            fmt::format("load tag value for \"{}\"", struct_type->name)
                    });
                    add(Instruction{
                            COMP_EQ,
                            {temp_registers[0], temp_registers[1], temp_registers[2]},
                            "compare with actual tag value"
                    });
                    add(Instruction{
                            JUMP_GT,
                            {temp_registers[2], LabelArgument{ labels[i] }},
                            "jump to the label if the values compare equal"
                    });
                    ++i;
                }

                add(Instruction{ DEBUG_BREAK, {}, "no variant could be matched" });

                assert(labels.size() == custom_type->struct_types.size());
                for (usize i = 0; const auto& pair : custom_type->struct_types) {
                    add(Label{ labels[i] });
                    pop_struct_from_stack_into_pointer(
                            pointer, pair.second, temp_registers[0], origin_location, label_generator, offset
                    );
                    add(Instruction{ JUMP, { LabelArgument{ end_label } } });
                    ++i;
                }

                add(Label{ end_label });
            }
        } else {
            assert(false and "not implemented");
        }
    }

    void Bssembly::push_value_onto_stack(
            const Register source_pointer,
            const DataType* data_type,
            const Location origin_location,
            const usize offset
    ) {
        using enum Register;
        using enum Mnemonic;

        const auto temp_register = (source_pointer == R1 ? R2 : R1);

        if (data_type->is_primitive_type() or data_type->is_pointer_type() or data_type->is_function_pointer_type()) {
            assert(data_type->size() <= WordSize);
            if (data_type->size() > 0) {
                const auto instruction = offset_copy_instruction_from_size(data_type->size());
                add(Instruction{
                        instruction,
                        {Pointer{ source_pointer }, Immediate{ offset }, temp_register},
                        origin_location,
                });
                add(Instruction{
                        PUSH,
                        { temp_register },
                        origin_location,
                });
            }
        } else if (data_type->is_array_type()) {
            const auto array_type = *(data_type->as_array_type());
            const auto size = array_type->contained->size();
            for (usize i = 0; i < array_type->num_elements; ++i) {
                push_value_onto_stack(source_pointer, array_type->contained, origin_location, offset + i * size);
            }
        } else if (data_type->is_struct_type()) {
            const auto struct_type = *(data_type->as_struct_type());
            const auto contains_tag = struct_type->contains_tag();

            if (contains_tag) {
                add(Instruction{
                        OFFSET_COPY,
                        {Pointer{ source_pointer }, Immediate{ offset }, temp_register},
                        "get the value of the tag",
                        origin_location
                });
                add(Instruction{ PUSH, { temp_register }, "push the value of the tag", origin_location });
            }

            usize current_offset = offset + (contains_tag ? WordSize : 0);
            for (const auto& attribute : struct_type->members) {
                current_offset = Utils::round_up(current_offset, attribute.data_type->alignment());
                push_value_onto_stack(source_pointer, attribute.data_type, origin_location, current_offset);
                current_offset += attribute.data_type->size();
            }
        } else {
            assert(false and "not implemented");
        }
    }

    void Bssembly::push_onto_stack_from_stack_pointer(
            const Register stack_pointer,
            const DataType* data_type,
            const Location origin_location,
            const usize offset
    ) {
        using enum Mnemonic;
        using enum Register;

        const auto temp_register = (stack_pointer == R1 ? R2 : R1);

        assert(offset % WordSize == 0);

        if (data_type->is_primitive_type() or data_type->is_pointer_type() or data_type->is_function_pointer_type()) {
            assert(data_type->size_when_pushed() == 0 or data_type->size_when_pushed() == WordSize);
            if (data_type->size_when_pushed() == WordSize) {
                add(Instruction{
                        OFFSET_COPY,
                        {Pointer{ stack_pointer }, Immediate{ offset }, temp_register},
                        origin_location,
                });
                add(Instruction{
                        PUSH,
                        { temp_register },
                        origin_location,
                });
            }
        } else if (data_type->is_array_type()) {
            const auto array_type = *(data_type->as_array_type());
            usize current_offset = offset;
            for (usize i = 0; i < array_type->num_elements; ++i) {
                push_onto_stack_from_stack_pointer(
                        stack_pointer, array_type->contained, origin_location, current_offset
                );
                current_offset += array_type->contained->size_when_pushed();
            }
        } else if (data_type->is_struct_type()) {
            const auto struct_type = *(data_type->as_struct_type());

            const auto contains_tag = struct_type->contains_tag();
            if (contains_tag) {
                add(Instruction{
                        OFFSET_COPY,
                        {stack_pointer, Immediate{ offset }, temp_register},
                        "get the value of the tag",
                        origin_location
                });
                add(Instruction{ PUSH, { temp_register }, "push the value of the tag", origin_location });
            }

            usize current_offset = offset + (contains_tag ? WordSize : 0);
            for (const auto& attribute : struct_type->members) {
                push_onto_stack_from_stack_pointer(stack_pointer, attribute.data_type, origin_location, current_offset);
                current_offset += attribute.data_type->size_when_pushed();
            }
        } else {
            assert(false and "not implemented");
        }
    }

    [[nodiscard]] Mnemonic Bssembly::offset_copy_instruction_from_size(const usize size) {
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

    void Bssembly::pop_struct_from_stack_into_pointer(
            const Register pointer,
            const StructType* struct_type,
            const Register temp_register,
            const Location origin_location,
            Emitter::LabelGenerator& label_generator,
            const usize offset
    ) {
        using enum Register;
        using enum Mnemonic;

        const auto num_members = struct_type->members.size();

        auto offsets = std::vector<usize>{};
        offsets.reserve(num_members);

        const auto padding_bytes = struct_type->size_when_pushed() - struct_type->size_when_pushed_without_padding();
        assert(padding_bytes % WordSize == 0);
        for (usize i = 0; i < padding_bytes; i += 4) {
            add(Instruction{ POP, {}, "discard struct padding" });
        }

        const auto contains_tag = struct_type->contains_tag();

        usize current_offset = offset + (contains_tag ? WordSize : 0);
        for (const auto& member : struct_type->members) {
            current_offset = Utils::round_up(current_offset, member.data_type->alignment());
            offsets.push_back(current_offset);
            current_offset += member.data_type->size();
        }

        for (usize i = 0; i < num_members; ++i) {
            const usize index = num_members - i - 1;
            pop_from_stack_into_pointer(
                    pointer, struct_type->members[index].data_type, origin_location, label_generator, offsets[index]
            );
        }

        if (contains_tag) {
            add(Instruction{
                    POP,
                    { temp_register },
                    "pop struct tag off the stack",
                    origin_location,
            });
            const auto instruction = offset_copy_instruction_from_size(WordSize);
            add(Instruction{
                    instruction,
                    {
                      temp_register, Immediate{ offset },
                      Pointer{ pointer },
                      },
                    origin_location,
            });
        }
    }

} // namespace Bssembler
