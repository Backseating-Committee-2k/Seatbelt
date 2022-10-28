//
// Created by coder2k on 24.06.2022.
//

#include "emitter.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "namespace.hpp"
#include "parser.hpp"
#include "types.hpp"
#include "upholsterer.hpp"
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
            void operator()(const Plus& token) const {
                visitor->bssembly.add(Instruction{
                        ADD,
                        {R1, R2, R3},
                        "add values",
                        token.location,
                });
            }

            void operator()(const Minus& token) const {
                visitor->bssembly.add(Instruction{
                        SUB,
                        {R1, R2, R3},
                        "subtract values",
                        token.location,
                });
            }

            void operator()(const Asterisk& token) const {
                visitor->bssembly.add(Instruction{
                        MULT,
                        {R1, R2, R4, R3},
                        "multiply values",
                        token.location,
                });
            }

            void operator()(const ForwardSlash& token) const {
                visitor->bssembly.add(Instruction{
                        DIVMOD,
                        {R1, R2, R3, R4},
                        "divmod the values, R4 gets the result of the division",
                        token.location,
                });
            }

            void operator()(const Mod& token) const {
                visitor->bssembly.add(Instruction{
                        DIVMOD,
                        {R1, R2, R4, R3},
                        "divmod the values, R3 gets the result of 'mod'",
                        token.location,
                });
            }

            void operator()(const And& token) const {
                visitor->bssembly.add(Instruction{
                        AND,
                        {R1, R2, R3},
                        "and values",
                        token.location,
                });
            }

            void operator()(const Or& token) const {
                visitor->bssembly.add(Instruction{
                        OR,
                        {R1, R2, R3},
                        "or values",
                        token.location,
                });
            }

            void operator()(const EqualsEquals& token) const {
                visitor->bssembly.add(Instruction{
                        COMP_EQ,
                        {R1, R2, R3},
                        token.location,
                });
            }

            void operator()(const ExclamationEquals& token) const {
                visitor->bssembly.add(Instruction{
                        COMP_NEQ,
                        {R1, R2, R3},
                        token.location,
                });
            }

            void operator()(const GreaterThan& token) const {
                visitor->bssembly.add(Instruction{
                        COMP_GT,
                        {R1, R2, R3},
                        token.location,
                });
            }

            void operator()(const GreaterOrEquals& token) const {
                visitor->bssembly.add(Instruction{
                        COMP_GE,
                        {R1, R2, R3},
                        token.location,
                });
            }

            void operator()(const LessThan& token) const {
                visitor->bssembly.add(Instruction{
                        COMP_LT,
                        {R1, R2, R3},
                        token.location,
                });
            }

            void operator()(const LessOrEquals& token) const {
                visitor->bssembly.add(Instruction{
                        COMP_LE,
                        {R1, R2, R3},
                        token.location,
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
            bssembly.add(Instruction{ PUSH,
                                      { Immediate{ expression.emittable_string } },
                                      "push immediate onto stack",
                                      expression.value.location });
        }

        void visit(Char& expression) override {
            bssembly.add(Instruction{ PUSH,
                                      { Immediate{ char_token_to_u8(expression.value) } },
                                      "push immediate onto stack",
                                      expression.value.location });
        }

        void visit(Bool& expression) override {
            const u8 value = expression.value.location.view() == "true" ? 1 : 0;
            bssembly.add(
                    Instruction{ PUSH, { Immediate{ value } }, "push immediate onto stack", expression.value.location }
            );
        }

        void visit(ArrayLiteral& expression) override {
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

        void visit_struct_or_custom_type_literal(auto& expression, const StructType* struct_type) {
            static constexpr auto is_custom_type_literal =
                    std::same_as<std::remove_cvref_t<decltype(expression)>, CustomTypeLiteral>;
            static constexpr auto is_struct_literal =
                    std::same_as<std::remove_cvref_t<decltype(expression)>, StructLiteral>;

            static_assert(is_custom_type_literal != is_struct_literal);

            if (struct_type->contains_tag()) {
                assert(struct_type->owning_custom_type_definition != nullptr
                       and "if the struct type is tagged it must have an owning type");
                const auto tag = struct_type->tag();
                assert(tag.has_value());
                bssembly.add(Instruction{ PUSH, { Immediate{ *tag } }, "push the tag of the struct onto the stack" });
            }
            usize bytes_pushed = (struct_type->contains_tag() ? WordSize : 0);
            for (const auto& initializer : expression.values) {
                initializer.field_value->accept(*this);
                bytes_pushed += initializer.field_value->data_type->size_when_pushed();
            }

            const auto total_bytes_to_push = expression.data_type->size_when_pushed();
            while (bytes_pushed < total_bytes_to_push) {
                bssembly.add(Instruction{ PUSH, { Immediate{ 255 } }, "additional struct padding" });
                bytes_pushed += WordSize;
            }
        }

        void visit(StructLiteral& expression) override {
            assert(expression.data_type->is_struct_type());
            const auto struct_type = *(expression.data_type->as_struct_type());
            visit_struct_or_custom_type_literal(expression, struct_type);
        }

        void visit(CustomTypeLiteral& expression) override {
            const auto tag = expression.definition->tag_by_struct_name(
                    Error::token_location(expression.type_name.name_tokens.back()).view()
            );
            assert(tag.has_value() and "the scope generator should've caught this before");
            assert(expression.definition->data_type->is_custom_type());
            const auto custom_type = *(expression.definition->data_type->as_custom_type());
            assert(custom_type->struct_types.contains(*tag));
            const auto struct_type = custom_type->struct_types.at(*tag);
            visit_struct_or_custom_type_literal(expression, struct_type);
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
                auto label_name = fmt::format("$\"{}\"", mangled_name);
                bssembly.add(Instruction{ PUSH,
                                          { Immediate{ std::move(label_name) } },
                                          "push address of label onto stack",
                                          std::get<Identifier>(expression.name_tokens.back()).location });
                return;
            }
            assert(expression.variable_symbol.has_value() and "if this is not a function, it has to be a variable");

            // we know, we have a variable

            const auto offset = expression.variable_symbol.value()->offset.value();
            const auto& variable_token = expression.name_tokens.back();
            const auto variable_name = Error::token_location(variable_token).view();

            // If the variable is used as an lvalue, we have to push its address onto the stack. Otherwise,
            // we dereference the address and push the value.
            if (expression.is_lvalue()) {
                bssembly.add(Instruction{
                        ADD,
                        {R0, Immediate{ offset }, R1},
                        fmt::format("calculate address of variable \"{}\"", variable_name),
                        std::get<Identifier>(expression.name_tokens.back()).location
                });
                bssembly.add(Instruction{ PUSH,
                                          { R1 },
                                          "push the address of variable onto the stack",
                                          std::get<Identifier>(expression.name_tokens.back()).location });
            } else {
                // we have an rvalue
                bssembly.add(Comment{
                        fmt::format("load value of variable \"{}\" and push it onto the stack", variable_name) });
                bssembly.push_value_onto_stack(
                        R0, expression.data_type, std::get<Identifier>(expression.name_tokens.back()).location, offset
                );
            }
        }

        void visit(UnaryOperator& expression) override {
            expression.operand->accept(*this);
            if (is<Not>(expression.operator_token)) {
                assert(expression.data_type == type_container->get_bool() and "type checker should've caught this");
                // we can assume that the value on the stack is 0 or 1 (representing false or true)
                // we have to flip the least significant bit to invert the truth value
                bssembly.add(Instruction{
                        POP,
                        { R1 },
                        "get value of operand for logical not operation",
                        Error::token_location(expression.operator_token),
                });
                bssembly.add(Instruction{
                        COPY,
                        {Immediate{ 1 }, R3},
                        "get constant 1",
                        Error::token_location(expression.operator_token),
                });
                bssembly.add(Instruction{
                        XOR,
                        {R1, R3, R1},
                        "flip the truth value",
                        Error::token_location(expression.operator_token),
                });
                bssembly.add(Instruction{
                        PUSH,
                        { R1 },
                        "push the result of the logical not operation",
                        Error::token_location(expression.operator_token),
                });
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
                    bssembly.add(Instruction{
                            POP,
                            { R1 },
                            "get address to dereference",
                            Error::token_location(expression.operator_token),
                    });
                    bssembly.push_value_onto_stack(
                            R1, expression.data_type, Error::token_location(expression.operator_token)
                    );
                }
            } else {
                assert(false and "not implemented");
            }
        }

        void short_circuiting_and(BinaryOperator& expression, Token operator_token) {
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R1)", Error::token_location(operator_token).view()),
                    Error::token_location(operator_token),
            });
            const auto end_of_evaluation = label_generator->next_label("end_of_short_circuiting");
            bssembly.add(Instruction{
                    JUMP_EQ,
                    {R1, Immediate{ end_of_evaluation }},
                    "skip rest of evaluation if value is false",
                    Error::token_location(operator_token),
            });
            bssembly.add(Instruction{
                    PUSH,
                    { R1 },
                    fmt::format(
                            "push left operand for {}-operator onto the stack",
                            Error::token_location(operator_token).view()
                    ),
                    Error::token_location(operator_token),
            });
            expression.rhs->accept(*this);
            bssembly.add(Instruction{
                    POP,
                    { R2 },
                    fmt::format(R"(store rhs for {}-operator in R2)", Error::token_location(operator_token).view()),
                    Error::token_location(operator_token),
            });
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R2)", Error::token_location(operator_token).view()),
                    Error::token_location(operator_token),
            });
            assert(expression.lhs->data_type == type_container->get_bool());
            assert(expression.rhs->data_type == type_container->get_bool());
            std::visit(UnsignedIntegralBinaryOperatorEmitter{ this }, operator_token);
            const auto after_push = label_generator->next_label("after_push");
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ after_push } },
                    Error::token_location(operator_token),
            });
            bssembly.add(Bssembler::Label{ end_of_evaluation });
            bssembly.add(Instruction{
                    COPY,
                    {R1, R3},
                    "store \"false\" as result",
                    Error::token_location(operator_token),
            });
            bssembly.add(Bssembler::Label{ after_push });
        }

        void short_circuiting_or(BinaryOperator& expression, Token operator_token) {
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R1)", Error::token_location(operator_token).view()),
                    Error::token_location(operator_token),
            });
            const auto end_of_evaluation = label_generator->next_label("end_of_short_circuiting");
            bssembly.add(Instruction{
                    JUMP_GT,
                    {R1, Immediate{ end_of_evaluation }},
                    "skip rest of evaluation if value is true",
                    Error::token_location(operator_token),
            });
            bssembly.add(Instruction{
                    PUSH,
                    { R1 },
                    fmt::format(
                            "push left operand for {}-operator onto the stack",
                            Error::token_location(operator_token).view()
                    ),
                    Error::token_location(operator_token),
            });
            expression.rhs->accept(*this);
            bssembly.add(Instruction{
                    POP,
                    { R2 },
                    fmt::format(R"(store rhs for {}-operator in R2)", Error::token_location(operator_token).view()),
                    Error::token_location(operator_token),
            });
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    fmt::format(R"(store lhs for {}-operator in R2)", Error::token_location(operator_token).view()),
                    Error::token_location(operator_token),
            });
            assert(expression.lhs->data_type == type_container->get_bool());
            assert(expression.rhs->data_type == type_container->get_bool());
            std::visit(UnsignedIntegralBinaryOperatorEmitter{ this }, operator_token);
            const auto after_push = label_generator->next_label("after_push");
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ after_push } },
                    Error::token_location(operator_token),
            });
            bssembly.add(Bssembler::Label{ end_of_evaluation });
            bssembly.add(Instruction{
                    COPY,
                    {R1, R3},
                    "store \"true\" as result",
                    Error::token_location(operator_token),
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
                        fmt::format(R"(store rhs for {}-operator in R2)", Error::token_location(operator_token).view()),
                        Error::token_location(operator_token),
                });
                bssembly.add(Instruction{
                        POP,
                        { R1 },
                        fmt::format(R"(store lhs for {}-operator in R1)", Error::token_location(operator_token).view()),
                        Error::token_location(operator_token),
                });
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
                        fmt::format("get size of data type \"{}\"", pointer_type->contained->to_string()),
                        Error::token_location(operator_token),
                });
                bssembly.add(Instruction{
                        MULT,
                        {Immediate{ number_register }, R4, R3, Immediate{ number_register }},
                        "multiply operand with size",
                        Error::token_location(operator_token),
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
                        fmt::format("get the size of the data type \"{}\"", pointer_type->to_string()),
                        Error::token_location(operator_token),
                });
                bssembly.add(Instruction{
                        DIVMOD,
                        {R3, R5, R3, R4},
                        "divide the result of pointer subtraction by the size of the data type",
                        Error::token_location(operator_token),
                });
            }
        }

        [[nodiscard]] usize get_attribute_offset(const StructType* struct_type, const std::string_view attribute_name) {
            const auto find_iterator = std::find_if(
                    struct_type->members.cbegin(), struct_type->members.cend(),
                    [&](const auto& attribute) { return attribute.name == attribute_name; }
            );
            [[maybe_unused]] const auto found = (find_iterator != struct_type->members.cend());
            assert(found and "this should have been caught before");
            return *(find_iterator->offset);
        }

        void visit(BinaryOperator& expression) override {
            using namespace Lexer::Tokens;

            expression.lhs->accept(*this);

            const auto operator_token = std::get_if<Token>(&expression.operator_type);

            if (operator_token != nullptr) {
                const auto is_attribute_access = std::holds_alternative<Dot>(*operator_token);
                if (is_attribute_access) {
                    const auto struct_is_lvalue = expression.lhs->is_lvalue();
                    const auto struct_is_rvalue = not struct_is_lvalue;
                    assert(expression.lhs->data_type->is_struct_type());
                    const auto attribute_name_expression = dynamic_cast<const Name*>(expression.rhs.get());
                    assert(attribute_name_expression != nullptr);
                    assert(attribute_name_expression->name_tokens.size() == 1);
                    const auto struct_type = *(expression.lhs->data_type->as_struct_type());
                    const auto attribute_name =
                            Error::token_location(attribute_name_expression->name_tokens.back()).view();
                    const auto attribute_offset = get_attribute_offset(struct_type, attribute_name);

                    assert((struct_is_lvalue or not expression.is_lvalue()) and "should have been caught before");

                    if (struct_is_lvalue and expression.is_rvalue()) {
                        /* Since the struct is an lvalue, the address of the struct has been pushed onto
                         * the stack. We now have to calculate the address of the struct attribute and
                         * fetch its value to put it onto the stack. */
                        bssembly.add(Instruction{
                                POP,
                                { R1 },
                                "get address of struct variable",
                                Error::token_location(*operator_token),
                        });
                        bssembly.push_value_onto_stack(
                                R1, expression.data_type, Error::token_location(*operator_token), attribute_offset
                        );
                    } else if (struct_is_rvalue and expression.is_rvalue()) {
                        /* The struct value has been pushed onto the stack (in its expanded form). We now
                         * calculate where the attribute of interest lies at and remember its address in
                         * R1. We then subtract from the stack pointer to invalidate the struct data (but it's
                         * still there, above the current stack).
                         * We then take the address we just remembered as a source pointer to push the attribute
                         * value onto the stack. */
                        usize expanded_offset = 0;
                        for (const auto& attribute : struct_type->members) {
                            if (attribute.name == attribute_name) {
                                break;
                            }
                            expanded_offset += attribute.data_type->size_when_pushed();
                            assert(expanded_offset % WordSize == 0);
                        }
                        assert(expanded_offset % WordSize == 0);
                        bssembly.add(Instruction{
                                SUB,
                                {SP, Immediate{ expression.lhs->data_type->size_when_pushed() - expanded_offset },
                                  R1},
                                fmt::format(
                                        R"(calculate address of member "{}" of the struct literal of type "{}")",
                                        attribute_name, expression.lhs->data_type->to_string()
                                ),
                                Error::token_location(*operator_token),
                        });
                        bssembly.add(Instruction{
                                SUB,
                                {SP, Immediate{ expression.lhs->data_type->size_when_pushed() }, SP},
                                fmt::format(
                                        "decrement stack pointer to discard rvalue of type \"{}\"",
                                        expression.lhs->data_type->to_string()
                                ),
                                Error::token_location(*operator_token),
                        });
                        bssembly.push_onto_stack_from_stack_pointer(
                                R1, expression.data_type, Error::token_location(*operator_token)
                        );
                    } else if (struct_is_lvalue and expression.is_lvalue()) {
                        bssembly.add(Instruction{
                                POP,
                                { R1 },
                                fmt::format(
                                        "get the address of the struct of type \"{}\"",
                                        expression.lhs->data_type->to_string()
                                ),
                                Error::token_location(*operator_token),
                        });
                        bssembly.add(Instruction{
                                ADD,
                                {R1, Immediate{ attribute_offset }, R1},
                                fmt::format("get the address of the attribute \"{}\"", attribute_name),
                                Error::token_location(*operator_token),
                        });
                        bssembly.add(Instruction{
                                PUSH,
                                { R1 },
                                fmt::format("push the address of the attribute \"{}\"", attribute_name),
                                Error::token_location(*operator_token),
                        });
                    } else {
                        assert(false and "unreachable");
                    }
                } else {
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
                            bssembly.add(Instruction{
                                    PUSH,
                                    { Immediate{ 1 } },
                                    "nothing == nothing yields true",
                                    Error::token_location(*operator_token),
                            });
                        } else if (is<ExclamationEquals>(*token)) {
                            bssembly.add(Instruction{
                                    PUSH,
                                    { Immediate{ 0 } },
                                    "nothing != nothing yields false",
                                    Error::token_location(*operator_token),
                            });
                        } else {
                            assert(false and "unreachable");
                        }
                    } else if (size <= WordSize) {
                        bssembly.add(Instruction{
                                PUSH,
                                { R3 },
                                "push result onto stack",
                                Error::token_location(*operator_token),
                        });
                    } else {
                        assert(false and "not implemented");
                    }
                }
            } else if (const auto index_operator = std::get_if<Parser::IndexOperator>(&expression.operator_type)) {
                assert(expression.lhs->data_type->is_array_type());
                bssembly.add(Comment{ "evaluate index of index operator" });
                expression.rhs->accept(*this);
                bssembly.add(Instruction{
                        POP,
                        { R1 },
                        "get index value",
                        index_operator->left_square_bracket_token.location,
                });

                // R1 holds the index value

                if (expression.lhs->is_lvalue()) {
                    /* The address of the array has been pushed onto the stack. We now have to calculate
                     * the offset of the element relative to the array start and then push the offset
                     * onto the stack. */
                    const auto contained_size = (*(expression.lhs->data_type->as_array_type()))->contained->size();
                    bssembly.add(Instruction{
                            COPY,
                            {Immediate{ contained_size }, R2},
                            "get size of contained data type",
                            index_operator->left_square_bracket_token.location,
                    });
                    bssembly.add(Instruction{
                            MULT,
                            {R1, R2, R4, R3},
                            "multiply index with size of contained data type",
                            index_operator->left_square_bracket_token.location,
                    });
                    bssembly.add(Instruction{
                            POP,
                            { R1 },
                            "get address of array",
                            index_operator->left_square_bracket_token.location,
                    });
                    bssembly.add(Instruction{
                            ADD,
                            {R1, R3, R1},
                            "get address of array element",
                            index_operator->left_square_bracket_token.location,
                    });
                    if (expression.is_lvalue()) {
                        bssembly.add(Instruction{
                                PUSH,
                                { R1 },
                                "push address of array element",
                                index_operator->left_square_bracket_token.location,
                        });
                    } else {
                        bssembly.push_value_onto_stack(
                                R1, expression.data_type, index_operator->left_square_bracket_token.location
                        );
                    }
                } else if (expression.lhs->is_rvalue() and expression.is_rvalue()) {
                    /* The whole array has been pushed onto the stack (in expanded form). We have to calculate
                     * the address of the member we want to access and store this address temporarily in a register.
                     * Then we decrement the stack pointer to discard the array data. We then push the one array
                     * element we need (pointed to by the saved address) onto the stack. */
                    const auto contained_size_when_pushed =
                            (*(expression.lhs->data_type->as_array_type()))->contained->size_when_pushed();
                    bssembly.add(Instruction{
                            COPY,
                            {Immediate{ contained_size_when_pushed }, R2},
                            "get size of contained data type in expanded form",
                            index_operator->left_square_bracket_token.location,
                    });
                    bssembly.add(Instruction{
                            MULT,
                            {R1, R2, R4, R3},
                            "multiply index with expanded size of contained data type",
                            index_operator->left_square_bracket_token.location,
                    });
                    const auto total_array_size = expression.lhs->data_type->size_when_pushed();
                    bssembly.add(Instruction{
                            SUB,
                            {SP, Immediate{ total_array_size }, SP},
                            "decrement stack pointer to discard array",
                            index_operator->left_square_bracket_token.location,
                    });
                    bssembly.add(Instruction{
                            ADD,
                            {SP, R3, R3},
                            "calculate address of array element inside the discarded stack",
                            index_operator->left_square_bracket_token.location,
                    });
                    bssembly.push_onto_stack_from_stack_pointer(
                            R3, (*(expression.lhs->data_type->as_array_type()))->contained,
                            index_operator->left_square_bracket_token.location, 0
                    );
                } else {
                    assert(false and "unreachable");
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
            const auto size_when_pushed = expression.data_type->size_when_pushed();
            if (return_value_into_pointer) {
                assert(size_when_pushed % WordSize == 0);
                bssembly.add(Instruction{
                        ADD,
                        {SP, Immediate{ size_when_pushed }, SP},
                        "reserve stack space for the return value",
                        expression.left_parenthesis.location,
                });
            }

            expression.callee->accept(*this); // evaluate callee => jump address is pushed

            const auto arguments_size = expression.arguments_size();
            /* We want to manipulate the stack pointer using the arguments_size, but the stack pointer may only ever
             * move in steps of the WordSize. Therefore, we may have to include padding. */
            const auto arguments_size_with_padding = Utils::round_up(arguments_size, WordSize);
            const auto arguments_padding = arguments_size_with_padding - arguments_size;

            if (return_value_into_pointer) {
                bssembly.add(Instruction{
                        SUB,
                        {SP, Immediate{ size_when_pushed + WordSize },
                          R1}, // WordSize because of the callee address that has been pushed
                        expression.left_parenthesis.location,
                });
                bssembly.add(Instruction{
                        OFFSET_COPY,
                        {R1, Immediate{ WordSize }, Pointer{ SP }},
                        "save address for return value as hidden first argument",
                        expression.left_parenthesis.location,
                });
            }
            bssembly.add(Instruction{
                    ADD,
                    {SP, Immediate{ arguments_size_with_padding + WordSize }, SP},
                    fmt::format(
                            "reserve stack space for the arguments (additional padding of {} bytes) + 1 extra word",
                            arguments_padding
                    ),
                    expression.left_parenthesis.location,
            });
            bssembly.add(Comment{ "evaluate all arguments one by one and put them into the reserved stack space" });

            usize current_offset = (return_value_into_pointer ? WordSize : 0);
            for (const auto& argument : expression.arguments) {
                if (argument->data_type->size() == 0) {
                    continue;
                }
                current_offset = Utils::round_up(current_offset, argument->data_type->alignment());
                bssembly.add(Comment{ "evaluate argument => result will be pushed" });
                argument->accept(*this);

                bssembly.add(Instruction{
                        SUB,
                        {SP,
                          Immediate{ argument->data_type->size_when_pushed() + arguments_size_with_padding
                          - current_offset },
                          R2},
                        "calculate target address",
                        expression.left_parenthesis.location,
                });
                bssembly.pop_from_stack_into_pointer(
                        R2, argument->data_type, expression.left_parenthesis.location, *label_generator
                );

                current_offset += argument->data_type->size();
            }

            bssembly.add(Instruction{
                    SUB,
                    {SP, Immediate{ arguments_size_with_padding + WordSize }, SP},
                    "reset stack pointer to where it was before",
                    expression.left_parenthesis.location,
            });

            /* By popping the address of the callee off of the stack, we will then have 2 words of free space inside
             * the stack right below the function arguments. */
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    "get the address of the callee",
                    expression.left_parenthesis.location,
            });

            /* The following CALL instruction occupies one of the empty words with the return address. The other
             * empty word will be filled by the callee with the old stack frame base pointer. */
            bssembly.add(Instruction{
                    CALL,
                    { R1 },
                    "call the function",
                    expression.left_parenthesis.location,
            });

            /* If the return value is not returned through a pointer and its size is bigger than zero,
             * we have to put the return value onto the stack. */
            if (expression.data_type->size() > 0 and not return_value_into_pointer) {
                bssembly.add(Instruction{
                        PUSH,
                        { R1 },
                        "push the return value of the called function",
                        expression.left_parenthesis.location,
                });
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
                bssembly.add(Instruction{
                        POP,
                        { R2 },
                        "get address of assignee",
                        expression.equals_token.location,
                });
                bssembly.add(Instruction{
                        SUB,
                        {SP, Immediate{ size_when_pushed }, R1},
                        "calculate address of value",
                        expression.equals_token.location,
                });
                bssembly.copy_from_stack_into_pointer(
                        R1, R2, expression.value->data_type, expression.equals_token.location
                );

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
                    fmt::format("get size of \"{}\"", expression.contained_data_type->to_string()),
                    expression.type_size_token.location,
            });
            bssembly.add(Instruction{
                    PUSH,
                    { R1 },
                    "push size onto the stack",
                    expression.type_size_token.location,
            });
        }

        void visit(ValueSizeExpression& expression) override {
            assert(expression.data_type != nullptr and "data type must have been set before");
            bssembly.add(Instruction{
                    COPY,
                    {Immediate{ expression.expression->data_type->size() }, R1},
                    fmt::format("get size of \"{}\"", expression.expression->data_type->to_string()),
                    expression.value_size_token.location,
            });
            bssembly.add(Instruction{
                    PUSH,
                    { R1 },
                    "push size onto the stack",
                    expression.value_size_token.location,
            });
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
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    "get result of condition",
                    statement.if_token.location,
            });
            bssembly.add(Instruction{
                    COPY,
                    {Immediate{ 0 }, R2},
                    "get constant zero",
                    statement.if_token.location,
            });
            bssembly.add(Instruction{
                    COMP,
                    {R1, R2, R3},
                    "evaluate if condition is true",
                    statement.if_token.location,
            });
            const auto else_label = label_generator->next_label("else");
            const auto endif_label = label_generator->next_label("endif");
            bssembly.add(Instruction{
                    JUMP_EQ,
                    {R3, Immediate{ else_label }},
                    "jump to else-block",
                    statement.if_token.location,
            });
            bssembly.add(Comment{ "begin of then-block" });
            statement.then_block.accept(*this);
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ endif_label } },
                    "jump to end of else-block",
                    statement.if_token.location,
            });
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
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ loop_start_label } },
                    statement.loop_token.location,
            });
            bssembly.add(Bssembler::Label{ loop_end_label, "end of loop" });
        }

        void visit(WhileStatement& statement) override {
            const auto while_start_label = label_generator->next_label("while_start");
            const auto while_condition_label = label_generator->next_label("while_condition");
            const auto while_end_label = label_generator->next_label("while_end");
            loop_stack.push(LoopLabels{ .continue_to_label{ while_condition_label },
                                        .break_to_label{ while_end_label } });
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ while_condition_label } },
                    "jump to condition of while-loop",
                    statement.while_token.location,
            });
            bssembly.add(Bssembler::Label{ while_start_label });

            statement.body.accept(*this);

            bssembly.add(Bssembler::Label{ while_condition_label });
            statement.condition->accept(*this);
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    "get value of while-loop condition",
                    statement.while_token.location,
            });

            bssembly.add(Instruction{
                    JUMP_GT,
                    {R1, Immediate{ while_start_label }},
                    "repeat the while-loop if the condition is true",
                    statement.while_token.location,
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
            bssembly.add(Instruction{
                    POP,
                    { R1 },
                    "get value of do-while-loop condition",
                    statement.while_token.location,
            });

            bssembly.add(Instruction{
                    JUMP_GT,
                    {R1, Immediate{ do_while_start_label }},
                    "repeat the do-while-loop if the condition is true",
                    statement.while_token.location,
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
                bssembly.add(Instruction{
                        JUMP,
                        { Immediate{ for_condition_label } },
                        "jump to the condition of the for-loop",
                        statement.for_token.location,
                });
            }

            bssembly.add(Bssembler::Label{ for_start_label });

            statement.body.accept(*this);

            if (statement.increment) {
                statement.increment->accept(*this);
                bssembly.add(Instruction{
                        POP,
                        { R1 },
                        "pop result of increment",
                        statement.for_token.location,
                });
            }

            bssembly.add(Bssembler::Label{ for_condition_label });
            if (statement.condition) {
                statement.condition->accept(*this);
                bssembly.add(Instruction{
                        POP,
                        { R1 },
                        "get value of for-loop condition",
                        statement.for_token.location,
                });
                bssembly.add(Instruction{
                        JUMP_GT,
                        {R1, Immediate{ for_start_label }},
                        "jump to the beginning of the for-loop",
                        statement.for_token.location,
                });
            } else {
                bssembly.add(Instruction{
                        JUMP,
                        { Immediate{ for_start_label } },
                        "jump to the beginning of the for-loop",
                        statement.for_token.location,
                });
            }
            bssembly.add(Bssembler::Label{ for_end_label });
            loop_stack.pop();
        }

        void visit(BreakStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.break_token, "break statement not allowed here");
            }
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ loop_stack.top().break_to_label } },
                    "break out of loop",
                    statement.break_token.location,
            });
        }

        void visit(ContinueStatement& statement) override {
            if (loop_stack.empty()) {
                Error::error(statement.continue_token, "continue statement not allowed here");
            }
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ loop_stack.top().continue_to_label } },
                    "continue to top of loop",
                    statement.continue_token.location,
            });
        }

        void visit(ReturnStatement& statement) override {
            if (statement.return_value) {
                // evaluate return value => result is pushed (if not of type Nothing)
                statement.return_value->accept(*this);

                if (statement.return_value->data_type->size() > 0) {
                    const auto return_value_into_pointer =
                            (statement.return_value->data_type->size_when_pushed() > WordSize);
                    if (return_value_into_pointer) {
                        bssembly.add(Instruction{
                                COPY,
                                {Pointer{ R0 }, R1},
                                "get address where to store the return value at",
                                statement.return_token.location,
                        });
                        const auto size_when_pushed = statement.return_value->data_type->size_when_pushed();
                        assert(size_when_pushed % WordSize == 0);
                        bssembly.pop_from_stack_into_pointer(R1, size_when_pushed, statement.return_token.location);
                    } else {
                        bssembly.add(Instruction{
                                POP,
                                { R1 },
                                "put return value into R1",
                                statement.return_token.location,
                        });
                    }
                }
            }
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ return_label } },
                    "immediately exit the current function",
                    statement.return_token.location,
            });
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
                        "get target address",
                        statement.let_token.location,
                });
                assert(statement.initial_value->data_type->alignment() <= WordSize and "unreachable");
                bssembly.pop_from_stack_into_pointer(
                        R1, statement.initial_value->data_type, statement.let_token.location, *label_generator
                );
            }
        }

        void visit(Parser::Statements::InlineAssembly& statement) override {
            const auto assembly_block = statement.token.location.view();
            const auto start_iterator = std::find(assembly_block.cbegin(), assembly_block.cend(), '{');
            assert(start_iterator != assembly_block.cend());
            const auto inner = std::string_view{ start_iterator + 1, assembly_block.cend() - 1 };
            const auto instructions = upholsterer2k::parse_bssembly(
                    statement.token.location.source_code.filename, inner, statement.token.location
            );
            bssembly.add(Comment{ "-- block of inline bssembly --" });
            for (const auto& instruction : instructions) {
                bssembly.add(instruction);
            }
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
                    bssembly.add(Instruction{
                            POP,
                            {},
                            statement.semicolon_token.location,
                    });
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
            bssembly.add(Instruction{
                    JUMP,
                    { Immediate{ label } },
                    fmt::format("goto {}", statement.label_identifier.location.view()),
                    statement.goto_token.location,
            });
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
        result.add(Instruction{
                PUSH,
                { R0 },
                "save the old stack frame base pointer",
                function_definition->name.location,
        });
        result.add(Instruction{
                COPY,
                {SP, R0},
                "set the new stack frame base pointer",
                function_definition->name.location,
        });
        result.add(Instruction{
                ADD,
                {SP, Immediate{ function_definition->occupied_stack_space.value() }, SP},
                "reserve stack space for arguments and local variables (arguments already filled by the caller)",
                function_definition->name.location,
        });

        // generate labels for all label definitions in the current function
        for (auto& label : function_definition->contained_labels) {
            label->emitted_label = label_generator->next_label(label->identifier.location.view());
        }

        const auto function_return_label = label_generator->next_label("function_return");

        result += emit_statement(function_definition->body, label_generator, function_return_label, type_container);

        result.add(Bssembler::Label{ function_return_label });

        result.add(Instruction{
                COPY,
                {R0, SP},
                "clear current stack frame",
                function_definition->name.location,
        });
        result.add(Instruction{
                POP,
                { R0 },
                "restore previous stack frame",
                function_definition->name.location,
        });
        result.add(Instruction{
                RETURN,
                {},
                function_definition->name.location,
        });

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
