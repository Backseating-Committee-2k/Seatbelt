//
// Created by coder2k on 16.10.2022.
//

#include "bssembly.hpp"


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

    void Bssembly::pop_from_stack_into_pointer(
            const Register pointer,
            const DataType* data_type,
            const Location origin_location,
            const usize offset
    ) {
        using enum Register;
        using enum Mnemonic;

        Register temp_register = (pointer == R1 ? R2 : R1);
        assert(temp_register != pointer);

        if (data_type->is_primitive_type() or data_type->is_pointer_type() or data_type->is_function_pointer_type()) {
            assert(data_type->size() <= WordSize);
            if (data_type->size() > 0) {
                add(Instruction{
                        POP,
                        { temp_register },
                        origin_location,
                });
                const auto instruction = offset_copy_instruction_from_size(data_type->size());
                add(Instruction{
                        instruction,
                        {
                          temp_register, Immediate{ offset },
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
                pop_from_stack_into_pointer(pointer, array_type->contained, origin_location, new_offset);
            }
        } else if (data_type->is_struct_type()) {
            const auto struct_type = *(data_type->as_struct_type());
            const auto num_members = struct_type->members.size();

            auto offsets = std::vector<usize>{};
            offsets.reserve(num_members);

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
                        pointer, struct_type->members[index].data_type, origin_location, offsets[index]
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

} // namespace Bssembler
