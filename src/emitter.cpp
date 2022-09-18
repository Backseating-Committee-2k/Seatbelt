//
// Created by coder2k on 24.06.2022.
//

#include "emitter.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "namespace.hpp"
#include "parser.hpp"
#include "types.hpp"
#include "utils.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <stack>
#include <string_view>

[[nodiscard]] static u8 char_token_to_u8(const Lexer::Tokens::CharLiteral& token) {
    const auto view = token.location.view();
    assert(view.front() == '\'');
    assert(view.back() == '\'');
    const auto is_escaped = (view[1] == '\\');
    assert((is_escaped and view.size() == 4) or (not is_escaped and view.size() == 3));
    if (not is_escaped) {
        return static_cast<u8>(view[1]);
    }
    switch (view[2]) {
        case '\'':
        case '\\':
            return static_cast<u8>(view[2]);
        case 't':
            return static_cast<u8>('\t');
        case 'n':
            return static_cast<u8>('\n');
        case 'v':
            return static_cast<u8>('\v');
        case 'f':
            return static_cast<u8>('\f');
        case 'r':
            return static_cast<u8>('\r');
        case '0':
            return 0;
        default:
            Error::error(token, "invalid escape sequence");
            return 0;
    }
}

namespace Emitter {
    using namespace Parser::Statements;
    using namespace Parser::Expressions;
    using namespace Bssembler;
    using enum Register;
    using enum Mnemonic;

    struct LoopLabels {
        std::string continue_to_label;
        std::string break_to_label;
    };

    using namespace Lexer::Tokens;

    struct EmitterVisitor : public ExpressionVisitor, public StatementVisitor {

        // This visitor emits instructions to handle binary operators for unsigned
        // integral data types, including pointers.
        // Operands are already stored in R1 and R2, result has to be stored in R3.
        struct UnsignedIntegralBinaryOperatorEmitter {
            void operator()(const Plus&) const {
                visitor->bssembly.add(Instruction{
                        ADD,
                        {R1, R2, R3},
                        "add values"
                });
            }

            void operator()(const Minus&) const {
                visitor->bssembly.add(Instruction{
                        SUB,
                        {R1, R2, R3},
                        "subtract values"
                });
            }

            void operator()(const Asterisk&) const {
                visitor->bssembly.add(Instruction{
                        MULT,
                        {R1, R2, R4, R3},
                        "multiply values"
                });
            }

            void operator()(const ForwardSlash&) const {
                visitor->bssembly.add(Instruction{
                        DIVMOD,
                        {R1, R2, R3, R4},
                        "divmod the values, R4 gets the result of the division"
                });
            }

            void operator()(const Mod&) const {
                visitor->bssembly.add(Instruction{
                        DIVMOD,
                        {R1, R2, R4, R3},
                        "divmod the values, R3 gets the result of 'mod'"
                });
            }

            void operator()(const And&) const {
                visitor->bssembly.add(Instruction{
                        AND,
                        {R1, R2, R3},
                        "and values"
                });
            }

            void operator()(const Or&) const {
                visitor->bssembly.add(Instruction{
                        OR,
                        {R1, R2, R3},
                        "or values"
                });
            }

            void operator()(const EqualsEquals&) const {
                visitor->bssembly.add(Instruction{
                        COMP_EQ,
                        {R1, R2, R3}
                });
            }

            void operator()(const ExclamationEquals&) const {
                visitor->bssembly.add(Instruction{
                        COMP_NEQ,
                        {R1, R2, R3}
                });
            }

            void operator()(const GreaterThan&) const {
                visitor->bssembly.add(Instruction{
                        COMP_GT,
                        {R1, R2, R3}
                });
            }

            void operator()(const GreaterOrEquals&) const {
                visitor->bssembly.add(Instruction{
                        COMP_GE,
                        {R1, R2, R3}
                });
            }

            void operator()(const LessThan&) const {
                visitor->bssembly.add(Instruction{
                        COMP_LT,
                        {R1, R2, R3}
                });
            }

            void operator()(const LessOrEquals&) const {
                visitor->bssembly.add(Instruction{
                        COMP_LE,
                        {R1, R2, R3}
                });
            }

            void operator()(const auto&) const {
                assert(false && "unreachable");
            }

            EmitterVisitor* visitor;
        };


        EmitterVisitor(TypeContainer* type_container, LabelGenerator* label_generator, std::string_view return_label)
            : type_container{ type_container },
              label_generator{ label_generator },
              return_label{ return_label } { }

        void visit(Integer& expression) override {
            assert(not expression.emittable_string.empty());
            bssembly.add(Instruction{ PUSH, { Immediate{ expression.emittable_string } }, "push immediate onto stack" }
            );
        }

        void visit(Char& expression) override {
            bssembly.add(Instruction{ PUSH,
                                      { Immediate{ char_token_to_u8(expression.value) } },
                                      "push immediate onto stack" });
        }

        void visit(Bool& expression) override {
            const u8 value = expression.value.location.view() == "true" ? 1 : 0;
            bssembly.add(Instruction{ PUSH, { Immediate{ value } }, "push immediate onto stack" });
        }

        void visit(Parser::Expressions::ArrayLiteral& expression) override {
            std::visit(
                    overloaded{ [&](const std::vector<std::unique_ptr<Expression>>& values) {
                                   for (const auto& value : values) {
                                       value->accept(*this);
                                   }
                               },
                                [&](const std::pair<std::unique_ptr<Expression>, usize>& pair) {
                                    for (usize i = 0; i < pair.second; ++i) {
                                        pair.first->accept(*this);
                                    }
                                } },
                    expression.values
            );
        }

        void visit(Name& expression) override {
            assert(expression.data_type && "data type must be known at this point");
            const auto is_function = expression.possible_overloads.has_value();
            if (is_function) {
                assert(expression.possible_overloads.value().size() == 1);
                const auto& overload = expression.possible_overloads.value().front();
                const auto mangled_name = fmt::format(
                        "{}{}", get_absolute_namespace_qualifier(*(overload->surrounding_namespace)),
                        overload->signature
                );
                bssembly.add(Instruction{
                        COPY,
                        {Immediate{ fmt::format("$\"{}\"", mangled_name) }, R1},
                        "get address of label"
                });
                bssembly.add(Instruction{ PUSH, { R1 }, "push address of label onto stack" });
                return;
            }
            assert(expression.variable_symbol.has_value() and "if this is not a function, it has to be a variable");

            // we know, we have a variable

            const auto offset = expression.variable_symbol.value()->offset.value();
            const auto& variable_token = expression.name_tokens.back();
            const auto variable_name = Error::token_location(variable_token).view();

            bssembly.add(Instruction{
                    ADD,
                    {R0, Immediate{ offset }, R1},
                    fmt::format("calculate address of variable \"{}\"", variable_name)
            });
            // If the variable is used as an lvalue, we have to push its address onto the stack. Otherwise,
            // we dereference the address and push the value.

            if (expression.is_lvalue()) {
                bssembly.add(Instruction{ PUSH, { R1 }, "push the address of variable onto the stack" });
            } else {
                // we have an rvalue
                bssembly.add(Comment{
                        fmt::format("load value of variable \"{}\" and push it onto the stack", variable_name) });
                bssembly.push_value_onto_stack(R1, expression.data_type);
            }
        }

        void visit(Parser::Expressions::UnaryOperator& expression) override {
            expression.operand->accept(*this);
            if (is<Not>(expression.operator_token)) {
                assert(expression.data_type == type_container->get_bool() and "type checker should've caught this");
                // we can assume that the value on the stack is 0 or 1 (representing false or true)
                // we have to flip the least significant bit to invert the truth value
                bssembly.add(Instruction{ POP, { R1 }, "get value of operand for logical not operation" });
                bssembly.add(Instruction{
                        COPY,
                        {Immediate{ 1 }, R3},
                        "get constant 1"
                });
                bssembly.add(Instruction{
                        XOR,
                        {R1, R3, R1},
                        "flip the truth value"
                });
                bssembly.add(Instruction{ PUSH, { R1 }, "push the result of the logical not operation" });
            } else if (is<At>(expression.operator_token)) {
                // we do not have to do anything, since the operand is guaranteed to be an lvalue and therefore
                // puts its address onto the stack upon being evaluated
            } else if (is<ExclamationMark>(expression.operator_token)) {
                // we have to act differently based on if the result of the dereferencing operation should be used
                // as an lvalue or as an rvalue
                if (expression.is_lvalue()) {
                    // lvalue: evaluating the operand already yielded the contained address, nothing to do here
                } else {
                    // rvalue: evaluating the operand yielded the address which we now have to dereference
                    bssembly.add(Instruction{ POP, { R1 }, "get address to dereference" });
                    bssembly.push_value_onto_stack(R1, expression.data_type);
                }
            } else {
                assert(false and "not implemented");
            }
        }

        void short_circuiting_and(BinaryOperator& expression, Token operator_token) {
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R1)", Error::token_location(operator_token).view()) });
            const auto end_of_evaluation = label_generator->next_label("end_of_short_circuiting");
            bssembly.add(Instruction{
                    JUMP_EQ,
                    {R1, Immediate{ end_of_evaluation }},
                    "skip rest of evaluation if value is false"
            });
            bssembly.add(Instruction{ PUSH,
                                      { R1 },
                                      fmt::format(
                                              "push left operand for {}-operator onto the stack",
                                              Error::token_location(operator_token).view()
                                      ) });
            expression.rhs->accept(*this);
            bssembly.add(Instruction{
                    POP,
                    { R2 },
                    fmt::format(R"(store rhs for {}-operator in R2)", Error::token_location(operator_token).view()) });
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R2)", Error::token_location(operator_token).view()) });
            assert(expression.lhs->data_type == type_container->get_bool());
            assert(expression.rhs->data_type == type_container->get_bool());
            std::visit(UnsignedIntegralBinaryOperatorEmitter{ this }, operator_token);
            const auto after_push = label_generator->next_label("after_push");
            bssembly.add(Instruction{ JUMP, { Immediate{ after_push } } });
            bssembly.add(Bssembler::Label{ end_of_evaluation });
            bssembly.add(Instruction{
                    COPY,
                    {R1, R3},
                    "store \"false\" as result"
            });
            bssembly.add(Bssembler::Label{ after_push });
        }

        void short_circuiting_or(BinaryOperator& expression, Token operator_token) {
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R1)", Error::token_location(operator_token).view()) });
            const auto end_of_evaluation = label_generator->next_label("end_of_short_circuiting");
            bssembly.add(Instruction{
                    JUMP_GT,
                    {R1, Immediate{ end_of_evaluation }},
                    "skip rest of evaluation if value is true"
            });
            bssembly.add(Instruction{ PUSH,
                                      { R1 },
                                      fmt::format(
                                              "push left operand for {}-operator onto the stack",
                                              Error::token_location(operator_token).view()
                                      ) });
            expression.rhs->accept(*this);
            bssembly.add(Instruction{
                    POP,
                    { R2 },
                    fmt::format(R"(store rhs for {}-operator in R2)", Error::token_location(operator_token).view()) });
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R2)", Error::token_location(operator_token).view()) });
            assert(expression.lhs->data_type == type_container->get_bool());
            assert(expression.rhs->data_type == type_container->get_bool());
            std::visit(UnsignedIntegralBinaryOperatorEmitter{ this }, operator_token);
            const auto after_push = label_generator->next_label("after_push");
            bssembly.add(Instruction{ JUMP, { Immediate{ after_push } } });
            bssembly.add(Bssembler::Label{ end_of_evaluation });
            bssembly.add(Instruction{
                    COPY,
                    {R1, R3},
                    "store \"true\" as result"
            });
            bssembly.add(Bssembler::Label{ after_push });
        }

        void non_short_circuiting_binary_operator(BinaryOperator& expression, Token operator_token) {
            expression.rhs->accept(*this);

            const auto size = expression.lhs->data_type->size();
            assert(expression.rhs->data_type->size() == size);

            if (size == 0) {
                // do nothing
            } else if (size <= WordSize) {
                bssembly.add(Instruction{
                        POP,
                        { R2 },
                        fmt::format(
                                R"(store rhs for {}-operator in R2)", Error::token_location(operator_token).view()
                        ) });
                bssembly.add(Instruction{
                        POP,
                        { R1 },
                        fmt::format(
                                R"(store lhs for {}-operator in R1)", Error::token_location(operator_token).view()
                        ) });
            } else {
                assert(false and "not implemented");
            }

            const auto exactly_one_is_a_pointer = (expression.lhs->data_type == type_container->get_u32()
                                                   and expression.rhs->data_type->is_pointer_type())
                                                  or (expression.lhs->data_type->is_pointer_type()
                                                      and expression.rhs->data_type == type_container->get_u32());
            const auto both_are_pointers =
                    (expression.lhs->data_type->is_pointer_type() and expression.rhs->data_type->is_pointer_type());

            if (exactly_one_is_a_pointer) {
                // one operand is a pointer, the other is a U32
                const auto first_is_pointer = expression.lhs->data_type->is_pointer_type();
                const auto& pointer = first_is_pointer ? expression.lhs : expression.rhs;
                const auto pointer_type = dynamic_cast<const PointerType*>(pointer->data_type);
                assert(pointer_type != nullptr);
                assert(is<Plus>(operator_token) or first_is_pointer);
                const auto contained_size = pointer_type->contained->size();
                const std::string_view number_register = (first_is_pointer ? "R2" : "R1");
                bssembly.add(Instruction{
                        COPY,
                        {Immediate{ contained_size }, R4},
                        fmt::format("get size of data type \"{}\"", pointer_type->contained->to_string())
                });
                bssembly.add(Instruction{
                        MULT,
                        {Immediate{ number_register }, R4, R3, Immediate{ number_register }},
                        "multiply operand with size"
                });
            }

            const auto is_same_type = (expression.lhs->data_type == expression.rhs->data_type);

            if (exactly_one_is_a_pointer or both_are_pointers
                or (is_same_type
                    and (expression.lhs->data_type == type_container->get_u32()
                         or expression.lhs->data_type == type_container->get_bool()
                         or expression.lhs->data_type == type_container->get_char()))) {
                std::visit(UnsignedIntegralBinaryOperatorEmitter{ this }, operator_token);
            } else if (expression.lhs->data_type == type_container->get_nothing()
                       and expression.rhs->data_type == type_container->get_nothing()) {
                // do nothing
            } else {
                assert(false and "not implemented");
            }

            if (both_are_pointers and is<Minus>(operator_token)) {
                const auto pointer_type = dynamic_cast<const PointerType*>(expression.lhs->data_type);
                assert(pointer_type != nullptr and "this must be a pointer type");
                const auto contained_size = pointer_type->contained->size();
                bssembly.add(Instruction{
                        COPY,
                        {Immediate{ contained_size }, R5},
                        fmt::format("get the size of the data type \"{}\"", pointer_type->to_string())
                });
                bssembly.add(Instruction{
                        DIVMOD,
                        {R3, R5, R3, R4},
                        "divide the result of pointer subtraction by the size of the data type"
                });
            }
        }

        void visit(BinaryOperator& expression) override {
            using namespace Lexer::Tokens;

            expression.lhs->accept(*this);

            if (const auto operator_token = std::get_if<Token>(&expression.operator_type)) {
                if (is<And>(*operator_token)) {
                    short_circuiting_and(expression, *operator_token);
                } else if (is<Or>(*operator_token)) {
                    short_circuiting_or(expression, *operator_token);
                } else {
                    non_short_circuiting_binary_operator(expression, *operator_token);
                }
                const auto size = expression.lhs->data_type->size();
                assert(expression.rhs->data_type->size() == size);
                if (size == 0) {
                    assert(expression.lhs->data_type == type_container->get_nothing()
                           and expression.rhs->data_type == type_container->get_nothing());
                    const auto token = std::get_if<Token>(&expression.operator_type);
                    assert(token != nullptr and "the type checker should've caught this");
                    if (is<EqualsEquals>(*token)) {
                        bssembly.add(Instruction{ PUSH, { Immediate{ 1 } }, "nothing == nothing yields true" });
                    } else if (is<ExclamationEquals>(*token)) {
                        bssembly.add(Instruction{ PUSH, { Immediate{ 0 } }, "nothing != nothing yields false" });
                    } else {
                        assert(false and "unreachable");
                    }
                } else if (size <= WordSize) {
                    bssembly.add(Instruction{ PUSH, { R3 }, "push result onto stack" });
                } else {
                    assert(false and "not implemented");
                }
            } else if (std::holds_alternative<Parser::IndexOperator>(expression.operator_type)) {
                assert(expression.lhs->is_lvalue());
                assert(expression.lhs->data_type->is_array_type());
                bssembly.add(Comment{ "evaluate index of index operator" });
                expression.rhs->accept(*this);
                bssembly.add(Instruction{ POP, { R1 }, "get index value" });
                bssembly.add(Instruction{
                        COPY,
                        {Immediate{ (*(expression.lhs->data_type->as_array_type()))->contained->size() }, R2},
                        "get size of contained data type"
                });
                bssembly.add(Instruction{
                        MULT,
                        {R1, R2, R4, R3},
                        "multiply index with size of contained data type"
                });
                bssembly.add(Instruction{ POP, { R1 }, "get address of array" });
                bssembly.add(Instruction{
                        ADD,
                        {R1, R3, R1},
                        "get address of array element"
                });
                if (expression.is_lvalue()) {
                    bssembly.add(Instruction{ PUSH, { R1 }, "push address of array element" });
                } else {
                    bssembly.push_value_onto_stack(R1, expression.data_type);
                }
            } else {
                assert(false and "not implemented");
            }
        }

        void visit(FunctionCall& expression) override {
            bssembly.add(Comment{ "evaluate function call" });

            /* If we call a function whose return type is too big to fit into a register, we
             * pass a pointer as hidden first argument. This pointer is then used to store the return
             * value. This implicitly increases the size of the arguments. */
            const auto return_value_into_pointer = expression.data_type->size_when_pushed() > WordSize;

            /*
             * Structure of stack before call-instruction:
             *
             * <arguments>
             * old stack frame base pointer
             * return address
             * jump address
             * stack space for the return value (only if it doesn't fit into a single register)
             */
            if (return_value_into_pointer) {
                const auto size_when_pushed = expression.data_type->size_when_pushed();
                assert(size_when_pushed % WordSize == 0);
                bssembly.add(Instruction{
                        ADD,
                        {SP, Immediate{ size_when_pushed }, SP},
                        "reserve stack space for the return value"
                });
            }

            expression.callee->accept(*this); // evaluate callee => jump address is pushed
            bssembly.add(Instruction{
                    ADD,
                    {SP, Immediate{ 4 }, SP},
                    "reserve stack space for the return address"
            });
            bssembly.add(Instruction{ PUSH, { R0 }, "push the current stack frame base pointer" });

            usize arguments_size = (return_value_into_pointer ? WordSize : 0);
            for (const auto& argument : expression.arguments) {
                const auto type = argument->data_type;
                arguments_size = Utils::round_up(arguments_size, type->alignment());
                arguments_size += type->size();
            }
            const auto arguments_size_with_padding = Utils::round_up(arguments_size, WordSize);
            const auto arguments_padding = arguments_size_with_padding - arguments_size;
            bssembly.add(Instruction{
                    ADD,
                    {SP, Immediate{ arguments_size_with_padding }, SP},
                    fmt::format(
                            "reserve stack space for the arguments (additional padding of {} bytes)", arguments_padding
                    )
            });
            bssembly.add(Comment{ "evaluate all arguments one by one and put them into the reserved stack space" });
            if (return_value_into_pointer) { }
            usize current_offset = (return_value_into_pointer ? WordSize : 0);
            for (const auto& argument : expression.arguments) {
                if (argument->data_type->size() == 0) {
                    continue;
                }
                current_offset = Utils::round_up(current_offset, argument->data_type->alignment());
                argument->accept(*this); // evaluate argument => result will be pushed
                bssembly.add(Instruction{
                        SUB,
                        {SP, Immediate{ argument->data_type->size_when_pushed() }, R1},
                        "calculate address of argument value"
                });
                bssembly.add(Instruction{
                        SUB,
                        {SP,
                          Immediate{ argument->data_type->size_when_pushed() + arguments_size_with_padding
                          - current_offset },
                          R2},
                        "calculate target address"
                });
                bssembly.add(Comment{ "mem-copy the argument" });
                bssembly.pop_from_stack_into_pointer(R2, argument->data_type);
                current_offset += argument->data_type->size();
            }

            // if we had to use padding for the arguments, we now have to undo the padding
            if (arguments_padding > 0) {
                bssembly.add(Instruction{
                        SUB,
                        {SP, Immediate{ arguments_padding }, SP},
                        fmt::format("undo the additional arguments padding of {} bytes", arguments_padding)
                });
            }

            bssembly.add(Instruction{
                    SUB,
                    {SP, Immediate{ arguments_size }, R0},
                    "set stack frame base pointer for callee"
            });

            if (return_value_into_pointer) {
                bssembly.add(Instruction{
                        SUB,
                        {R0, Immediate{ 12 + expression.data_type->size_when_pushed() }, R1},
                        "calculate address of return value"
                });
                bssembly.add(Instruction{
                        COPY,
                        {R1, Pointer{ R0 }},
                        "pass the address for the return value as hidden first parameter"
                });
            }

            bssembly.add(Instruction{
                    SUB,
                    {R0, Immediate{ 8 }, R1},
                    "calculate address of placeholder for return address"
            });
            const auto call_return_label = label_generator->next_label("return_address");
            bssembly.add(Instruction{
                    COPY,
                    {Immediate{ call_return_label }, R2},
                    "get return address"
            });
            bssembly.add(Instruction{
                    COPY,
                    {R2, Pointer{ R1 }},
                    "fill in return address"
            });
            bssembly.add(Instruction{
                    SUB,
                    {R0, Immediate{ 12 }, R1},
                    "calculate address of jump address"
            });
            bssembly.add(Instruction{
                    COPY,
                    {Pointer{ R1 }, R1},
                    "dereference the pointer"
            });
            bssembly.add(Instruction{ JUMP, { R1 }, "call the function" });
            bssembly.add(Bssembler::Label{ call_return_label,
                                           "this is where the control flow returns to after the function call" });
            bssembly.add(Instruction{ POP, {}, "pop the callee address off of the stack" });

            /* If the return value is not returned through a pointer and its size is bigger than zero,
             * we have to put the return value onto the stack. */
            if (expression.data_type->size() > 0 and not return_value_into_pointer) {
                bssembly.add(Instruction{ PUSH, { R1 }, "push the return value of the called function" });
            }
            /* If the return value is returned though a pointer, it now resides in the area we previously
             * reserved. This is the top of the stack.
             * The return value resides there in "expanded" form. E.g. an array like [Bool; 4] occupies
             * 4 * WordSize = 16 bytes on the stack. */
        }

        void visit(Assignment& expression) override {
            assert(expression.assignee->is_lvalue());

            const auto size_when_pushed = expression.data_type->size_when_pushed();
            if (size_when_pushed > 0) {
                bssembly.add(Comment{ "evaluate the right-hand-side of the assignment (put result on the stack)" });
                expression.value->accept(*this); // puts value on the stack
                bssembly.add(Comment{ "evaluate the assignee to get the address to store the value at" });
                expression.assignee->accept(*this); // this will put the address of the assignee onto the stack
                bssembly.add(Instruction{ POP, { R2 }, "get address of assignee" });
                bssembly.add(Instruction{
                        SUB,
                        {SP, Immediate{ size_when_pushed }, R1},
                        "calculate address of value"
                });
                //bssembly.emit_mem_copy(R1, R2, size, alignment);
                bssembly.copy_from_stack_into_pointer(R1, R2, expression.value->data_type);

                // Since assignments are expressions, they have a resulting value. This value must lie on the
                // stack after the assignment has been evaluated. Since we only copied the value, it is still
                // present on the stack. We don't have to do anything else here.
            }
        }

        void visit(TypeSizeExpression& expression) override {
            assert(expression.contained_data_type != nullptr and "data type must have been set before");
            bssembly.add(Instruction{
                    COPY,
                    {Immediate{ expression.contained_data_type->size() }, R1},
                    fmt::format("get size of \"{}\"", expression.contained_data_type->to_string())
            });
            bssembly.add(Instruction{ PUSH, { R1 }, "push size onto the stack" });
        }

        void visit(ValueSizeExpression& expression) override {
            assert(expression.data_type != nullptr and "data type must have been set before");
            bssembly.add(Instruction{
                    COPY,
                    {Immediate{ expression.expression->data_type->size() }, R1},
                    fmt::format("get size of \"{}\"", expression.expression->data_type->to_string())
            });
            bssembly.add(Instruction{ PUSH, { R1 }, "push size onto the stack" });
        }

        void visit(Nothing&) override {
            // we do not push anything onto the stack, since this data type has a size of 0 bytes
        }

        void visit(Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
        }

        void visit(IfStatement& statement) override {
            statement.condition->accept(*this);
            bssembly.add(Instruction{ POP, { R1 }, "get result of condition" });
            bssembly.add(Instruction{
                    COPY,
                    {Immediate{ 0 }, R2},
                    "get constant zero"
            });
            bssembly.add(Instruction{
                    COMP,
                    {R1, R2, R3},
                    "evaluate if condition is true"
            });
            const auto else_label = label_generator->next_label("else");
            const auto endif_label = label_generator->next_label("endif");
            bssembly.add(Instruction{
                    JUMP_EQ,
                    {R3, Immediate{ else_label }},
                    "jump to else-block"
            });
            bssembly.add(Comment{ "begin of then-block" });
            statement.then_block.accept(*this);
            bssembly.add(Instruction{ JUMP, { Immediate{ endif_label } }, "jump to end of else-block" });
            bssembly.add(Bssembler::Label{ else_label, "begin of else-block" });
            statement.else_block.accept(*this);
            bssembly.add(Bssembler::Label{ endif_label, "end of else-block" });
        }

        void visit(LoopStatement& statement) override {
            const auto loop_start_label = label_generator->next_label("loop_start");
            const auto loop_end_label = label_generator->next_label("loop_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ loop_start_label }, .break_to_label{ loop_end_label } });
            bssembly.add(Bssembler::Label{ loop_start_label, "brrrrr" });
            statement.body.accept(*this);
            loop_stack.pop();
            bssembly.add(Instruction{ JUMP, { Immediate{ loop_start_label } } });
            bssembly.add(Bssembler::Label{ loop_end_label, "end of loop" });
        }

        void visit(WhileStatement& statement) override {
            const auto while_start_label = label_generator->next_label("while_start");
            const auto while_condition_label = label_generator->next_label("while_condition");
            const auto while_end_label = label_generator->next_label("while_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ while_condition_label },
                                        .break_to_label{ while_end_label } });
            bssembly.add(Instruction{ JUMP, { Immediate{ while_condition_label } }, "jump to condition of while-loop" }
            );
            bssembly.add(Bssembler::Label{ while_start_label });

            statement.body.accept(*this);

            bssembly.add(Bssembler::Label{ while_condition_label });
            statement.condition->accept(*this);
            bssembly.add(Instruction{ POP, { R1 }, "get value of while-loop condition" });

            bssembly.add(Instruction{
                    JUMP_GT,
                    {R1, Immediate{ while_start_label }},
                    "repeat the while-loop if the condition is true"
            });

            bssembly.add(Bssembler::Label{ while_end_label });
            loop_stack.pop();
        }

        void visit(DoWhileStatement& statement) override {
            const auto do_while_start_label = label_generator->next_label("do_while_start");
            const auto do_while_condition_label = label_generator->next_label("do_while_condition");
            const auto do_while_end_label = label_generator->next_label("do_while_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ do_while_condition_label },
                                        .break_to_label{ do_while_end_label } });

            bssembly.add(Bssembler::Label{ do_while_start_label });
            statement.body.accept(*this);

            bssembly.add(Bssembler::Label{ do_while_condition_label });
            statement.condition->accept(*this);
            bssembly.add(Instruction{ POP, { R1 }, "get value of do-while-loop condition" });

            bssembly.add(Instruction{
                    JUMP_GT,
                    {R1, Immediate{ do_while_start_label }},
                    "repeat the do-while-loop if the condition is true"
            });

            bssembly.add(Bssembler::Label{ do_while_end_label });
            loop_stack.pop();
        }

        void visit(ForStatement& statement) override {
            const auto for_start_label = label_generator->next_label("for_start");
            const auto for_condition_label = label_generator->next_label("for_condition");
            const auto for_end_label = label_generator->next_label("for_end");

            loop_stack.push(LoopLabels{ .continue_to_label{ for_condition_label }, .break_to_label{ for_end_label } });

            if (statement.initializer) {
                statement.initializer->accept(*this);
            }

            if (statement.condition) {
                bssembly.add(Instruction{ JUMP,
                                          { Immediate{ for_condition_label } },
                                          "jump to the condition of the for-loop" });
            }

            bssembly.add(Bssembler::Label{ for_start_label });

            statement.body.accept(*this);

            if (statement.increment) {
                statement.increment->accept(*this);
                bssembly.add(Instruction{ POP, { R1 }, "pop result of increment" });
            }

            bssembly.add(Bssembler::Label{ for_condition_label });
            if (statement.condition) {
                statement.condition->accept(*this);
                bssembly.add(Instruction{ POP, { R1 }, "get value of for-loop condition" });
                bssembly.add(Instruction{
                        JUMP_GT,
                        {R1, Immediate{ for_start_label }},
                        "jump to the beginning of the for-loop"
                });
            } else {
                bssembly.add(
                        Instruction{ JUMP, { Immediate{ for_start_label } }, "jump to the beginning of the for-loop" }
                );
            }
            bssembly.add(Bssembler::Label{ for_end_label });
            loop_stack.pop();
        }

        void visit(BreakStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.break_token, "break statement not allowed here");
            }
            bssembly.add(Instruction{ JUMP, { Immediate{ loop_stack.top().break_to_label } }, "break out of loop" });
        }

        void visit(ContinueStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.continue_token, "continue statement not allowed here");
            }
            bssembly.add(
                    Instruction{ JUMP, { Immediate{ loop_stack.top().continue_to_label } }, "continue to top of loop" }
            );
        }

        void visit(ReturnStatement& statement) override {
            if (statement.return_value) {
                if (statement.return_value->data_type->size() > 0) {
                    statement.return_value->accept(*this); // evaluate return value => result is pushed

                    const auto return_value_into_pointer =
                            (statement.return_value->data_type->size_when_pushed() > WordSize);
                    if (return_value_into_pointer) {
                        bssembly.add(Instruction{
                                COPY,
                                {Pointer{ R0 }, R1},
                                "get address where to store the return value at"
                        });
                        const auto size_when_pushed = statement.return_value->data_type->size_when_pushed();
                        assert(size_when_pushed % WordSize == 0);
                        bssembly.pop_from_stack_into_pointer(R1, size_when_pushed);
                    } else {
                        bssembly.add(Instruction{ POP, { R1 }, "put return value into R1" });
                    }
                }
            }
            bssembly.add(Instruction{ JUMP, { Immediate{ return_label } }, "immediately exit the current function" });
        }

        void visit(VariableDefinition& statement) override {
            const usize offset =
                    std::get<VariableSymbol>((*statement.surrounding_scope).at(statement.name.location.view()))
                            .offset.value();
            bssembly.add(Comment{
                    fmt::format("new variable called \"{}\" with offset {}", statement.name.location.view(), offset) });

            statement.initial_value->accept(*this); // initial value is evaluated and pushed

            if (statement.initial_value->data_type->size() > 0) {
                assert(statement.variable_symbol->offset.has_value() and "offset must have been set before");
                bssembly.add(Instruction{
                        ADD,
                        {R0, Immediate{ *(statement.variable_symbol->offset) }, R1},
                        "get target address"
                });
                assert(statement.initial_value->data_type->alignment() <= WordSize and "unreachable");
                bssembly.pop_from_stack_into_pointer(R1, statement.initial_value->data_type);
            }
        }

        void visit(Parser::Statements::InlineAssembly& statement) override {
            const auto assembly_block = statement.token.location.view();
            const auto start_iterator = std::find(assembly_block.cbegin(), assembly_block.cend(), '{');
            assert(start_iterator != assembly_block.cend());
            const auto inner = std::string_view{ start_iterator + 1, assembly_block.cend() - 1 };
            bssembly.add(Comment{ "-- block of inline bssembly --" });
            bssembly.add(InlineBssembly{ inner });
            bssembly.add(NewLine{});
            bssembly.add(Comment{ "-- end of inline bssembly --" });
        }

        void visit(ExpressionStatement& statement) override {
            statement.expression->accept(*this);
            const auto size_when_pushed = statement.expression->data_type->size_when_pushed();
            assert(size_when_pushed % WordSize == 0);
            if (size_when_pushed > 0) {
                const auto num_words = size_when_pushed / WordSize;
                bssembly.add(Comment{ "discard value of expression statement" });
                for (usize i = 0; i < num_words; ++i) {
                    bssembly.add(Instruction{ POP, {} });
                }
            }
        }

        void visit(LabelDefinition& statement) override {
            assert(not statement.emitted_label.empty() and "emitted label must have been generated before");
            bssembly.add(Bssembler::Label{ statement.emitted_label });
        }

        void visit(GotoStatement& statement) override {
            assert(statement.target_label != nullptr and "target label must have been set before");
            const auto& label = statement.target_label->emitted_label;
            bssembly.add(Instruction{ JUMP,
                                      { Immediate{ label } },
                                      fmt::format("goto {}", statement.label_identifier.location.view()) });
        }

        TypeContainer* type_container;
        LabelGenerator* label_generator;
        std::stack<LoopLabels> loop_stack{};
        std::string_view return_label;
        Bssembler::Bssembly bssembly{};
    };

    Bssembly emit_statement(
            Statement& statement,
            LabelGenerator* label_generator,
            const std::string_view return_label,
            TypeContainer* type_container
    ) {
        auto visitor = EmitterVisitor{ type_container, label_generator, return_label };
        statement.accept(visitor);
        return std::move(visitor.bssembly);
    }

    Bssembly Emitter::operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
        assert(function_definition->occupied_stack_space.has_value() and "size of stack frame has to be known");
        assert(function_definition->parameters_stack_space.has_value() and "size of parameters has to be known");
        assert(function_definition->surrounding_scope != nullptr and "this has to be set before");
        assert(function_definition->surrounding_scope->surrounding_namespace != nullptr and "this has to be set before"
        );

        const auto mangled_name = fmt::format(
                "{}{}",
                get_absolute_namespace_qualifier(*(function_definition->surrounding_scope->surrounding_namespace)),
                function_definition->corresponding_symbol->signature
        );

        auto result = Bssembly{};
        result.add(NewLine{});
        result.add(Bssembler::Label{ fmt::format("$\"{}\"", mangled_name) });

        // get the size needed for the locals of this function (excluding its parameters)
        const auto locals_size =
                function_definition->occupied_stack_space.value() - function_definition->parameters_stack_space.value();

        // the caller has already pushed the arguments onto the stack - we only have to reserve stack space for
        // the locals of this function
        result.add(Instruction{
                ADD,
                {SP, Immediate{ locals_size }, SP},
                "reserve stack space for local variables"
        });

        // generate labels for all label definitions in the current function
        for (auto& label : function_definition->contained_labels) {
            label->emitted_label = label_generator->next_label(label->identifier.location.view());
        }

        if (function_definition->is_entry_point) {
            result.add(Instruction{
                    COPY,
                    {SP, R0},
                    "save stack frame base pointer for main function into R0"
            });
            assert(function_definition->body.occupied_stack_space.has_value()
                   and "needed stack size for function must be known");
            result.add(Instruction{
                    ADD,
                    {SP,
                      Immediate{ Utils::round_up(function_definition->body.occupied_stack_space.value(), WordSize) },
                      SP},
                    "reserve stack space for main function"
            });
        }
        const auto function_return_label = label_generator->next_label("function_return");

        result += emit_statement(function_definition->body, label_generator, function_return_label, type_container);

        result.add(Bssembler::Label{ function_return_label });

        if (function_definition->is_entry_point) {
            result.add(Instruction{ HALT, {} });
        } else {
            result.add(Instruction{
                    COPY,
                    {R0, SP},
                    "clear current stack frame"
            });
            result.add(Instruction{ POP, { R0 }, "restore previous stack frame" });
            result.add(Instruction{ RETURN, {} });
        }

        return result;
    }

    Bssembly Emitter::operator()(const std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
        auto result = Bssembly{};
        for (const auto& top_level_statement : namespace_definition->contents) {
            result += std::visit(*this, top_level_statement);
        }
        return result;
    }

} // namespace Emitter
