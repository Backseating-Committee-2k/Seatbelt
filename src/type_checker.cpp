//
// Created by coder2k on 10.07.2022.
//

#include "type_checker.hpp"
#include "error.hpp"
#include "namespace.hpp"
#include "return_type_checker.hpp"
#include "utils.hpp"
#include <algorithm>
#include <array>
#include <cassert>
#include <concepts>
#include <fmt/core.h>
#include <fmt/format.h>
#include <limits>
#include <ranges>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>

[[nodiscard]] u32 octal_to_decimal(std::string_view text) {
    if (text.starts_with("0o")) {
        text = text.substr(2);
    }
    u32 result = 0;
    u32 factor = 1;
    for (char c : std::ranges::reverse_view(text)) {
        assert(c >= '0' and c <= '7' and "digit out of bounds for octal number");
        const u32 digit = c - '0';
        result += factor * digit;
        factor *= 8;
    }
    return result;
}

namespace TypeChecker {
    using Lexer::Tokens::Token;

    [[nodiscard]] static DataType*
    get_resulting_data_type_for_arrays(ArrayType* lhs, const Token& token, ArrayType* rhs) {
        if (lhs->num_elements != rhs->num_elements) {
            return nullptr;
        }
        if (lhs->contained != rhs->contained) {
            return nullptr;
        }
        return is<Lexer::Tokens::Equals>(token) ? lhs : nullptr;
    }

    [[nodiscard]] static DataType* get_resulting_data_type_for_function_pointers(
            FunctionPointerType* lhs,
            const Token& token,
            FunctionPointerType* rhs,
            TypeContainer& type_container
    ) {
        using namespace Lexer::Tokens;
        if (lhs->parameter_types.size() != rhs->parameter_types.size()) {
            return nullptr;
        }
        for (usize i = 0; i < lhs->parameter_types.size(); ++i) {
            if (lhs->parameter_types[i] != rhs->parameter_types[i]) {
                return nullptr;
            }
        }
        if (lhs->return_type != rhs->return_type) {
            return nullptr;
        }

        if (is_one_of<EqualsEquals, ExclamationEquals>(token)) {
            return type_container.get_bool();
        }
        if (is<Equals>(token)) {
            return lhs;
        }
        return nullptr;
    }

    [[nodiscard]] static DataType* get_resulting_data_type_for_pointers(
            PointerType* lhs,
            const Token& token,
            PointerType* rhs,
            TypeContainer& type_container
    ) {
        using namespace Lexer::Tokens;
        if (lhs->contained != rhs->contained) {
            return nullptr;
        }
        if (is_one_of<EqualsEquals, ExclamationEquals, GreaterThan, GreaterOrEquals, LessThan, LessOrEquals>(token)) {
            return type_container.get_bool();
        }
        if (is<Equals>(token)
            and (lhs->binding_mutability == Mutability::Const or rhs->binding_mutability == Mutability::Mutable)) {
            return lhs;
        }
        if (is<Minus>(token)) {
            return type_container.get_u32(); // TODO: this should be a unique data type (e. g. "Distance")
        }
        return nullptr;
    }

    [[nodiscard]] static DataType*
    get_resulting_data_type(DataType* lhs, const Token& token, DataType* rhs, TypeContainer& type_container) {
        using namespace Lexer::Tokens;
        assert(lhs != nullptr);
        assert(rhs != nullptr);

        const auto first_struct_type = dynamic_cast<StructType*>(lhs);  // maybe nullptr
        const auto second_struct_type = dynamic_cast<StructType*>(rhs); // maybe nullptr
        if (first_struct_type != nullptr and second_struct_type != nullptr) {
            if (not std::holds_alternative<Equals>(token)) {
                return nullptr;
            }
            return (first_struct_type->operator==(*second_struct_type) ? first_struct_type : nullptr);
        }

        const auto first_array_type = dynamic_cast<ArrayType*>(lhs);  // maybe nullptr
        const auto second_array_type = dynamic_cast<ArrayType*>(rhs); // maybe nullptr
        if (first_array_type != nullptr and second_array_type != nullptr) {
            return get_resulting_data_type_for_arrays(first_array_type, token, second_array_type);
        }

        const auto first_function_pointer = dynamic_cast<FunctionPointerType*>(lhs);  // maybe nullptr
        const auto second_function_pointer = dynamic_cast<FunctionPointerType*>(rhs); // maybe nullptr

        if (first_function_pointer != nullptr and second_function_pointer != nullptr) {
            return get_resulting_data_type_for_function_pointers(
                    first_function_pointer, token, second_function_pointer, type_container
            );
        } else if (first_function_pointer != nullptr or second_function_pointer != nullptr) {
            return nullptr;
        }

        // if neither operand is a function pointer, we evaluate the actual types

        const auto same_type = (lhs == rhs);

        const auto first_pointer = dynamic_cast<PointerType*>(lhs);  // maybe nullptr
        const auto second_pointer = dynamic_cast<PointerType*>(rhs); // maybe nullptr
        if (first_pointer != nullptr and second_pointer != nullptr) {
            return get_resulting_data_type_for_pointers(first_pointer, token, second_pointer, type_container);
        }

        const auto primitive_types = std::array{ lhs->as_primitive_type(), rhs->as_primitive_type() };
        const auto both_primitive = (primitive_types[0].has_value() and primitive_types[1].has_value());
        if (is_one_of<EqualsEquals, ExclamationEquals>(token)) {
            if (not both_primitive or primitive_types[0].value() != primitive_types[1].value()) {
                return nullptr;
            }
            return type_container.get_bool();
        }
        if (is_one_of<GreaterThan, GreaterOrEquals, LessThan, LessOrEquals>(token)) {
            if (not both_primitive or *primitive_types[0] != *primitive_types[1]
                or *primitive_types[0] != type_container.get_u32()) {
                return nullptr;
            }
            return type_container.get_bool();
        }
        if (is<Equals>(token)) {
            if (not both_primitive or primitive_types[0].value() != primitive_types[1].value()) {
                return nullptr;
            }
            return rhs;
        }
        if (is_one_of<Plus, Minus>(token) and (first_pointer or second_pointer)) {
            // pointer arithmetics
            if (primitive_types[0].has_value() and *primitive_types[0] == type_container.get_u32()) {
                if (is<Plus>(token)) {
                    return second_pointer;
                }
            }
            if (primitive_types[1].has_value() and *primitive_types[1] == type_container.get_u32()) {
                return first_pointer;
            }
        }
        if (is_one_of<Plus, Minus, Asterisk, ForwardSlash, Mod>(token)) {
            if (not both_primitive or primitive_types[0].value() != primitive_types[1].value()) {
                return nullptr;
            }
            return *primitive_types[0] == type_container.get_u32() ? type_container.get_u32() : nullptr;
        }
        if (is_one_of<And, Or, Xor>(token)) {
            if (not same_type or not both_primitive) {
                return nullptr;
            }
            return *primitive_types[0] == type_container.get_bool() ? lhs : nullptr;
        }
        return nullptr;
    }

    [[nodiscard]] static DataType*
    get_resulting_data_type(const Parser::Expressions::BinaryOperator& expression, TypeContainer& type_container) {
        if (const auto operator_token = std::get_if<Token>(&expression.operator_type)) {
            return get_resulting_data_type(
                    expression.lhs->data_type, *operator_token, expression.rhs->data_type, type_container
            );
        } else if (const auto index_operator = std::get_if<Parser::IndexOperator>(&expression.operator_type)) {
            if (expression.lhs->data_type->is_array_type() and expression.rhs->data_type == type_container.get_u32()) {
                return (*(expression.lhs->data_type->as_array_type()))->contained;
            }
            return nullptr;
        } else {
            assert(false and "not implemented");
            return nullptr;
        }
    }

    /**
     * Takes a type definition and checks if it represents a custom type placeholder (which means that it
     * either is a placeholder for a CustomType or for a StructType) and then returns the actual data type
     * that corresponds to the placeholder. Assumes that the corresponding type already is defined and
     * contained in the type container, because this should've been checked by the scope generator before.
     * @param type_definition The definition of the type, e.g. statement.type_definition for a VariableDefinition
     *                        statement.
     * @return The data type as stored in the type container.
     */
    [[nodiscard]] static DataType*
    get_data_type_of_placeholder(TypeContainer* type_container, const DataType* type_definition) {
        if (type_definition->is_pointer_type()) {
            const auto pointer_type = *(type_definition->as_pointer_type());
            const auto contained_placeholder_type =
                    get_data_type_of_placeholder(type_container, pointer_type->contained);
            if (contained_placeholder_type == nullptr) {
                return nullptr;
            }
            return type_container->pointer_to(contained_placeholder_type, pointer_type->binding_mutability);
        }

        if (type_definition->is_function_pointer_type()) {
            const auto function_pointer_type = *(type_definition->as_function_pointer_type());
            auto was_replaced_as_least_once = false;
            auto parameter_types = std::vector<DataType*>{};
            parameter_types.reserve(function_pointer_type->parameter_types.size());
            for (const auto parameter_type : function_pointer_type->parameter_types) {
                const auto placeholder_type = get_data_type_of_placeholder(type_container, parameter_type);
                if (placeholder_type != nullptr) {
                    was_replaced_as_least_once = true;
                    parameter_types.push_back(placeholder_type);
                } else {
                    parameter_types.push_back(parameter_type);
                }
            }
            const auto placeholder_type =
                    get_data_type_of_placeholder(type_container, function_pointer_type->return_type);
            if (placeholder_type != nullptr) {
                was_replaced_as_least_once = true;
            }
            const auto return_type =
                    (placeholder_type == nullptr ? function_pointer_type->return_type : placeholder_type);
            if (was_replaced_as_least_once) {
                return type_container->function_pointer(std::move(parameter_types), return_type);
            }
            return nullptr;
        }

        if (type_definition->is_array_type()) {
            const auto array_type = *(type_definition->as_array_type());
            const auto contained_placeholder_type = get_data_type_of_placeholder(type_container, array_type->contained);
            if (contained_placeholder_type == nullptr) {
                return nullptr;
            }
            return type_container->array_of(contained_placeholder_type, array_type->num_elements);
        }

        if (type_definition->is_struct_type()) {
            const auto struct_type = *(type_definition->as_struct_type());
            auto attributes = std::vector<StructMember>{};
            attributes.reserve(struct_type->members.size());
            auto did_replace = false;
            for (const auto& attribute : struct_type->members) {
                const auto placeholder_type = get_data_type_of_placeholder(type_container, attribute.data_type);
                if (placeholder_type != nullptr) {
                    did_replace = true;
                    // TODO: check if the strings in here could be moved
                    attributes.push_back(
                            StructMember{ .name{ attribute.name }, .data_type{ placeholder_type }, .offset{} }
                    );
                } else {
                    // TODO: check if the strings in here could be moved
                    attributes.push_back(
                            StructMember{ .name{ attribute.name }, .data_type{ attribute.data_type }, .offset{} }
                    );
                }
            }
            if (not did_replace) {
                return nullptr;
            }
            // TODO: check if the strings in here could be moved
            return type_container->struct_of(
                    struct_type->name, struct_type->namespace_qualifier, struct_type->custom_type_name,
                    std::move(attributes)
            );
        }

        if (type_definition->is_custom_type()) {
            assert(false and "not implemented");
        }

        const auto is_custom_type_placeholder =
                (type_definition != nullptr and type_definition->is_custom_type_placeholder());
        if (is_custom_type_placeholder) {
            const auto placeholder_type = *(type_definition->as_custom_type_placeholder());
            const auto is_struct_type = (placeholder_type->struct_definition != nullptr);
            const auto is_custom_type = (placeholder_type->custom_type_definition != nullptr);
            assert(is_struct_type != is_custom_type); // XOR
            if (is_struct_type) {
                return placeholder_type->struct_definition->data_type;
            } else if (is_custom_type) {
                assert(false and "not implemented");
            } else {
                assert(false and "unreachable");
            }
        }

        if (type_definition->is_primitive_type()) {
            return nullptr;
        }

        assert(false and "unreachable");
        return nullptr;
    }

    template<typename... Types>
    [[nodiscard]] static bool holds_any_of(const Lexer::Tokens::Token& token) {
        return (std::holds_alternative<Types>(token) or ...);
    }

    struct TypeCheckerVisitor : public Parser::Statements::StatementVisitor,
                                public Parser::Expressions::ExpressionVisitor {
        TypeCheckerVisitor(TypeContainer* type_container, const Parser::FunctionDefinition* surrounding_function)
            : type_container{ type_container },
              surrounding_function{ surrounding_function } { }

        void visit(Parser::Statements::Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
        }

        void visit(Parser::Statements::IfStatement& statement) override {
            statement.condition->accept(*this);
            // condition must be an rvalue
            statement.condition->value_type = ValueType::RValue;

            const auto condition_type = dynamic_cast<const PrimitiveType*>(statement.condition->data_type);
            if (condition_type == nullptr or condition_type != type_container->get_bool()) {
                Error::error(
                        statement.if_token,
                        fmt::format(
                                "condition of if-statement must evaluate to a boolean expression (found type \"{}\")",
                                statement.condition->data_type->to_string()
                        )
                );
            }
            statement.then_block.accept(*this);
            statement.else_block.accept(*this);
        }

        void visit(Parser::Statements::LoopStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::BreakStatement&) override { }

        void visit(Parser::Statements::ContinueStatement&) override { }

        void visit(Parser::Statements::WhileStatement& statement) override {
            statement.condition->accept(*this);
            // condition must be an rvalue
            statement.condition->value_type = ValueType::RValue;

            const auto condition_type = dynamic_cast<const PrimitiveType*>(statement.condition->data_type);
            if (condition_type == nullptr or condition_type != type_container->get_bool()) {
                Error::error(
                        statement.while_token, fmt::format(
                                                       "condition of while-statement must evaluate to a boolean "
                                                       "expression (found type \"{}\")",
                                                       statement.condition->data_type->to_string()
                                               )
                );
            }
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::DoWhileStatement& statement) override {
            statement.body.accept(*this);
            statement.condition->accept(*this);
            // condition must be an rvalue
            statement.condition->value_type = ValueType::RValue;

            const auto condition_type = dynamic_cast<const PrimitiveType*>(statement.condition->data_type);
            if (condition_type == nullptr or condition_type != type_container->get_bool()) {
                Error::error(
                        statement.while_token, fmt::format(
                                                       "condition of do-while-statement must evaluate to a boolean "
                                                       "expression (found type \"{}\")",
                                                       statement.condition->data_type->to_string()
                                               )
                );
            }
        }

        void visit(Parser::Statements::ForStatement& statement) override {
            using Parser::Statements::VariableDefinition;
            if (statement.initializer) {
                statement.initializer->accept(*this);
            }
            if (statement.condition) {
                statement.condition->accept(*this);
                // condition must be an rvalue
                statement.condition->value_type = ValueType::RValue;

                const auto condition_type = dynamic_cast<const PrimitiveType*>(statement.condition->data_type);
                if (condition_type == nullptr or condition_type != type_container->get_bool()) {
                    Error::error(
                            statement.for_token, fmt::format(
                                                         "condition of for-statement must evaluate to a boolean "
                                                         "expression (found type \"{}\")",
                                                         statement.condition->data_type->to_string()
                                                 )
                    );
                }
            }
            if (statement.increment) {
                statement.increment->accept(*this);
                // increment must be an rvalue
                statement.increment->value_type = ValueType::RValue;
            }
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::ReturnStatement& statement) override {
            assert(surrounding_function);
            assert(surrounding_function->return_type and "return type must be known at this point");
            if (statement.return_value) {
                statement.return_value->accept(*this);
                // we can only return rvalues
                statement.return_value->value_type = ValueType::RValue;
            }
            const auto return_value_type =
                    statement.return_value ? statement.return_value->data_type : type_container->get_nothing();
            const auto return_value_type_mutable = return_value_type;
            const auto function_return_type_mutable = surrounding_function->return_type;
            if (return_value_type_mutable != function_return_type_mutable) {
                Error::error(
                        statement.return_token,
                        fmt::format(
                                R"(type of return value "{}" is incompatible with function return type "{}")",
                                return_value_type->to_string(), surrounding_function->return_type->to_string()
                        )
                );
            }
        }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            // if the type definition is missing, we have to do automatic type deduction
            const auto type_deduction = not static_cast<bool>(statement.type_definition);

            // get the type of the initial value
            statement.initial_value->accept(*this);
            statement.initial_value->value_type = ValueType::RValue; // initial value must be an rvalue

            if (not type_deduction) {
                const auto placeholder_data_type =
                        get_data_type_of_placeholder(type_container, statement.type_definition.get());
                if (placeholder_data_type != nullptr) {
                    statement.data_type = placeholder_data_type;
                } else {
                    // this is no custom type placeholder
                    auto& type = statement.type_definition;

                    if (not type_container->is_defined(*type)) {
                        assert(not statement.type_definition_tokens.empty()
                               and "if this is empty, we use automatic type deduction");
                        Error::error(
                                statement.type_definition_tokens.front(),
                                fmt::v9::format("use of undeclared type \"{}\"", type->to_string())
                        );
                    }
                    statement.data_type = type_container->from_type_definition(std::move(statement.type_definition));
                }
            } else {
                // when doing type deduction, we use the type of the initial value as type for the variable
                statement.data_type = statement.initial_value->data_type;
            }

            assert(statement.data_type and statement.initial_value->data_type and "missing type information");

            const auto assignee_type = statement.data_type;

            // type checking rules are the same as if we would do type checking during an assignment
            const auto resulting_type = get_resulting_data_type(
                    assignee_type, statement.equals_token, statement.initial_value->data_type, *type_container
            );

            if (resulting_type == nullptr) {
                Error::error(
                        statement.equals_token,
                        fmt::format(
                                R"(cannot initialize a variable of type "{}" with a value of type "{}")",
                                statement.data_type->to_string(), statement.initial_value->data_type->to_string()
                        )
                );
            }
        }

        void visit(Parser::Statements::InlineAssembly&) override { }

        void visit(Parser::Statements::ExpressionStatement& statement) override {
            statement.expression->accept(*this);
            // expression must be an rvalue
            statement.expression->value_type = ValueType::RValue;
        }

        void visit(Parser::Statements::LabelDefinition&) override { }

        void visit(Parser::Statements::GotoStatement&) override { }

        void visit(Parser::Expressions::Integer& expression) override {
            /* We know that the integer literal semantically is correct, but
             * it still may exceed the maximum possible value. First we have
             * to discard any underscores in the literal, and then we have to
             * validate the value.
             *
             * On a side note:   Not only do we validate the number here, but we
             * ~~~~~~~~~~~~~~~   also save the number as a string that can be
             *                   emitted. Of course this should not be the
             *                   responsibility of the type checker, but this way
             *                   we can prevent the need of checking/converting
             *                   the number twice. */
            auto number_string = Utils::strip_underscores(expression.value.location.view());
            const auto base = Utils::get_base(expression.value.location.view());
            const auto digits_view =
                    (base == 10 ? std::string_view{ number_string }
                                : std::string_view{ number_string.begin() + 2, number_string.end() });
            if (not Utils::validate_integer<u32>(digits_view, base)) {
                Error::error(expression.value, "integer literal out of bounds");
            }
            // The bssembler language does not support octal literals. We convert those to decimal first.
            expression.emittable_string =
                    (base == 8 ? std::to_string(octal_to_decimal(digits_view)) : std::move(number_string));

            expression.data_type = type_container->get_u32();
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::Char& expression) override {
            expression.data_type = type_container->get_char();
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::Bool& expression) override {
            expression.data_type = type_container->get_bool();
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::ArrayLiteral& expression) override {
            using namespace Parser::Expressions;
            std::visit(
                    overloaded{ [&](std::vector<std::unique_ptr<Expression>>& values) {
                                   for (auto& value : values) {
                                       value->accept(*this);
                                       assert(value->value_type != ValueType::Undetermined);
                                       value->value_type = ValueType::RValue;
                                   }
                                   assert(not values.empty() and "empty arrays are not allowed");
                                   const auto type = values.front()->data_type;
                                   for (usize i = 1; i < values.size(); ++i) {
                                       if (values[i]->data_type != type) {
                                           Error::error(*values[i], "conflicting data types in array literal");
                                       }
                                   }
                                   expression.data_type = type_container->array_of(type, values.size());
                               },
                                [&](std::pair<std::unique_ptr<Expression>, usize>& pair) {
                                    pair.first->accept(*this);
                                    assert(pair.first->value_type != ValueType::Undetermined);
                                    pair.first->value_type = ValueType::RValue;
                                    expression.data_type = type_container->array_of(pair.first->data_type, pair.second);
                                } },
                    expression.values
            );
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::StructLiteral& expression) override {
            using std::ranges::find_if;

            /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
             * check the types of the field initializers
             * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
            for (auto& initializer : expression.values) {
                initializer.field_value->accept(*this);
                assert(initializer.field_value->value_type != ValueType::Undetermined);
                initializer.field_value->value_type = ValueType::RValue;
            }

            /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
             * check if the struct literal matches the type it refers to
             * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
            const auto struct_definition = expression.definition;
            for (usize i = 0; i < std::min(expression.values.size(), struct_definition->members.size()); ++i) {
                const auto& literal_member = expression.values[i];
                const auto& struct_member = struct_definition->members[i];
                if (literal_member.field_name.location.view() != struct_member.name.location.view()) {
                    Error::error(
                            literal_member.field_name,
                            fmt::format(
                                    R"(expected initializer for attribute "{}: {}" (got "{}: {}"))",
                                    struct_member.name.location.view(), struct_member.type->to_string(),
                                    literal_member.field_name.location.view(),
                                    literal_member.field_value->data_type->to_string()
                            )
                    );
                }
                if (literal_member.field_value->data_type != struct_member.type) {
                    // TODO: improve error message by pointing to the data type instead of the initializer name
                    Error::error(
                            literal_member.field_name,
                            fmt::format(
                                    R"(type mismatch: expected "{}" for attribute "{}" (got "{}"))",
                                    struct_member.type->to_string(), literal_member.field_name.location.view(),
                                    literal_member.field_value->data_type->to_string()
                            )
                    );
                }
            }

            if (expression.values.size() > struct_definition->members.size()) {
                const auto& first_excess_value = expression.values[struct_definition->members.size()];
                Error::error(
                        first_excess_value.field_name,
                        fmt::format(
                                R"(too many initializers for struct "{}")", struct_definition->name.location.view(),
                                first_excess_value.field_name.location.view()
                        )
                );
            } else if (expression.values.size() < struct_definition->members.size()) {
                using std::ranges::views::transform, std::ranges::views::drop;
                const auto one_missing = (expression.values.size() + 1 == struct_definition->members.size());
                Error::error(
                        expression.type_name,
                        fmt::format(
                                "struct literal of type \"{}\" is missing {} for the {} {}",
                                Error::token_location(expression.type_name.name_tokens.back()).view(),
                                one_missing ? "an initializer" : "initializers",
                                one_missing ? "attribute" : "attributes",
                                fmt::join(
                                        struct_definition->members | drop(expression.values.size())
                                                | transform([](const auto& initializer) {
                                                      return fmt::format(
                                                              "\"{}: {}\"", initializer.name.location.view(),
                                                              initializer.type->to_string()
                                                      );
                                                  }),
                                        ", "
                                )
                        )
                );
            }

            expression.data_type = expression.definition->data_type;
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::Name& expression) override {
            using Parser::Statements::VariableDefinition;
            const auto is_variable = expression.variable_symbol.has_value();

            if (is_variable) {
                // now we visit the definition that is the origin of this variable
                const auto& definition = expression.variable_symbol.value()->definition;
                [[maybe_unused]] const auto name =
                        std::string{ Error::token_location(expression.name_tokens.back()).view() };
                expression.data_type = std::visit(
                        overloaded{ [&](const VariableDefinition* variable_definition) -> DataType* {
                                       assert(variable_definition->data_type and "type must be known");
                                       expression.value_type =
                                               (variable_definition->binding_mutability == Mutability::Mutable
                                                        ? ValueType::MutableLValue
                                                        : ValueType::ConstLValue);
                                       return variable_definition->data_type;
                                   },
                                    [&](const Parameter* parameter_definition) -> DataType* {
                                        assert(parameter_definition->data_type and "type must be known");
                                        expression.value_type =
                                                (parameter_definition->binding_mutability == Mutability::Mutable
                                                         ? ValueType::MutableLValue
                                                         : ValueType::ConstLValue);
                                        const auto placeholder_type = get_data_type_of_placeholder(
                                                type_container, parameter_definition->data_type
                                        );
                                        if (placeholder_type != nullptr) {
                                            return placeholder_type;
                                        }
                                        return parameter_definition->data_type;
                                    },
                                    [](std::monostate) -> DataType* {
                                        assert(false and "unreachable");
                                        return nullptr;
                                    } },
                        definition
                );
            } else {
                // a function always is an rvalue
                expression.value_type = ValueType::RValue;

                // only the last token of a qualified name is relevant for lookup
                const auto& name_token = expression.name_tokens.back();
                const auto identifier = Error::token_location(name_token).view();
                const auto symbol = scope_lookup(expression.surrounding_scope, identifier);
                if (symbol == nullptr) {
                    Error::error(name_token, fmt::format("use of undeclared identifier \"{}\"", identifier));
                }
                assert(expression.possible_overloads.has_value() and "overloads have to be determined beforehand");
                if (const auto function_symbol = std::get_if<FunctionSymbol>(symbol)) {
                    const auto& overloads = *(expression.possible_overloads);
                    assert(not overloads.empty() and "there shall never be a function with zero overloads");
                    if (overloads.size() > 1) {
                        Error::error(name_token, fmt::format("use of identifier \"{}\" is ambiguous", identifier));
                    }
                    assert(overloads.size() == 1);
                    const auto function_definition = overloads.front()->definition;
                    auto parameter_types = std::vector<DataType*>{};
                    parameter_types.reserve(function_definition->parameters.size());
                    for (const auto& parameter : function_definition->parameters) {
                        parameter_types.push_back(parameter.data_type);
                    }
                    expression.data_type = type_container->from_type_definition(std::make_unique<FunctionPointerType>(
                            std::move(parameter_types), function_definition->return_type
                    ));
                } else {
                    assert(false and "unreachable");
                }
            }
        }

        void visit(Parser::Expressions::UnaryOperator& expression) override {
            expression.operand->accept(*this);
            const auto operand_type = expression.operand->data_type;
            if (is<Lexer::Tokens::Not>(expression.operator_token)) {
                if (*operand_type != *type_container->get_bool()) {
                    Error::error(
                            expression.operator_token, fmt::format(
                                                               "logical 'not'-operator can only be applied to boolean "
                                                               "expressions (found type \"{}\")",
                                                               operand_type->to_string()
                                                       )
                    );
                }
                // the operand is required to be an rvalue
                expression.operand->value_type = ValueType::RValue;

                expression.data_type = operand_type;
                expression.value_type = ValueType::RValue;
            } else if (is<Lexer::Tokens::At>(expression.operator_token)) {
                if (not expression.operand->is_lvalue()) {
                    Error::error(expression.operator_token, "lvalue required to take the address of this expression");
                }
                const auto binding_mutability =
                        (expression.operand->value_type == ValueType::MutableLValue ? Mutability::Mutable
                                                                                    : Mutability::Const);
                expression.data_type = type_container->pointer_to(expression.operand->data_type, binding_mutability);
                expression.value_type = ValueType::RValue;
            } else if (is<Lexer::Tokens::ExclamationMark>(expression.operator_token)) {
                // the operand is required to be an rvalue
                expression.operand->value_type = ValueType::RValue;
                if (not expression.operand->data_type->is_pointer_type()) {
                    Error::error(
                            expression.operator_token,
                            fmt::format(
                                    "only pointer types can be dereferenced (found type \"{}\")",
                                    expression.operand->data_type->to_string()
                            )
                    );
                }
                const auto pointer_type = dynamic_cast<const PointerType*>(expression.operand->data_type);
                assert(pointer_type != nullptr and "we checked before that this is a pointer");
                expression.data_type = pointer_type->contained;
                // dereferencing a pointer yields an lvalue
                expression.value_type =
                        (pointer_type->binding_mutability == Mutability::Mutable ? ValueType::MutableLValue
                                                                                 : ValueType::ConstLValue);
            } else {
                assert(false and "not implemented");
            }
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            using namespace Lexer::Tokens;
            expression.lhs->accept(*this);

            // find out if this binary operator represents access to a struct attribute
            const auto token = std::get_if<Token>(&expression.operator_type); // maybe nullptr
            [[maybe_unused]] const auto is_attribute_access =
                    (token != nullptr and std::holds_alternative<Dot>(*token));

            /* attribute access must be handled differently to all other binary operators, because the
             * scope generator is unable to do a name lookup for the attribute before the type checker
             * has determined the type of the struct we want to access */
            if (is_attribute_access) {
                if (not expression.lhs->data_type->is_struct_type()) {
                    Error::error(
                            *token, fmt::format(
                                            "value of type \"{}\" does not offer attribute access",
                                            expression.lhs->data_type->to_string()
                                    )
                    );
                }
                const auto attribute_name_expression =
                        dynamic_cast<const Parser::Expressions::Name*>(expression.rhs.get());
                assert(attribute_name_expression != nullptr and "should have been caught by the parser");
                assert(attribute_name_expression->name_tokens.size() == 1 and "should have been caught by the parser");
                assert(expression.lhs->data_type->is_struct_type());

                const auto& struct_type = **(expression.lhs->data_type->as_struct_type());
                const auto& struct_attributes = struct_type.members;

                const auto find_iterator =
                        std::find_if(struct_attributes.cbegin(), struct_attributes.cend(), [&](const auto& attribute) {
                            return attribute.name
                                   == Error::token_location(attribute_name_expression->name_tokens.back()).view();
                        });
                const auto found = (find_iterator != struct_attributes.cend());
                if (not found) {
                    Error::error(
                            attribute_name_expression->name_tokens.back(),
                            fmt::format(
                                    R"(struct of type "{}" has no attribute "{}")", struct_type.name,
                                    Error::token_location(attribute_name_expression->name_tokens.back()).view()
                            )
                    );
                }
                expression.data_type = find_iterator->data_type;
                expression.value_type = expression.lhs->value_type;
            } else {
                expression.rhs->accept(*this);

                if (std::holds_alternative<Token>(expression.operator_type)) {
                    // both operands are required to be rvalues
                    expression.lhs->value_type = ValueType::RValue;
                    expression.rhs->value_type = ValueType::RValue;

                    // the result also is an rvalue
                    expression.value_type = ValueType::RValue;
                } else if (std::holds_alternative<Parser::IndexOperator>(expression.operator_type)) {
                    // for index operators, the whole expression inherits its value type from the left operand
                    expression.value_type = expression.lhs->value_type;

                    // the right operator must be an rvalue
                    expression.rhs->value_type = ValueType::RValue;
                } else {
                    assert(false and "not implemented");
                }

                if (const auto resulting_type = get_resulting_data_type(expression, *type_container)) {
                    expression.data_type = resulting_type;
                    return;
                }
                Error::error(
                        expression,
                        fmt::format(
                                R"(operator "{}" can not be applied to operands of type "{}" and "{}")",
                                Parser::binary_operator_type_to_string_view(expression.operator_type),
                                expression.lhs->data_type->to_string(), expression.rhs->data_type->to_string()
                        )
                );
            }
        }

        void visit(Parser::Expressions::FunctionCall& expression) override {
            using std::ranges::find_if;

            for (const auto& argument : expression.arguments) {
                argument->accept(*this);

                // all arguments must be rvalues
                argument->value_type = ValueType::RValue;
            }

            const auto name = dynamic_cast<Parser::Expressions::Name*>(expression.callee.get());
            const auto is_name = (name != nullptr);
            const auto is_function = (is_name and name->possible_overloads.has_value());
            if (not is_function) {
                // this could either be a function pointer or an expression that evaluates to a function pointer

                // first evaluate the callee - this is needed in either case
                expression.callee->accept(*this);
                // callee must be an rvalue
                expression.callee->value_type = ValueType::RValue;

                // this must be a function pointer
                const auto function_pointer_type =
                        dynamic_cast<const FunctionPointerType*>(expression.callee->data_type);
                if (function_pointer_type == nullptr) {
                    Error::error(
                            expression.left_parenthesis,
                            fmt::format(
                                    "unable to call an expression of type {}", expression.callee->data_type->to_string()
                            )
                    );
                }
                const auto expected_num_parameters = function_pointer_type->parameter_types.size();
                const auto actual_num_parameters = expression.arguments.size();
                if (actual_num_parameters != expected_num_parameters) {
                    Error::error(
                            name->name_tokens.back(),
                            fmt::format(
                                    "too {} arguments provided for function of type \"{}\" (got {}, expected {})",
                                    actual_num_parameters < expected_num_parameters ? "few" : "many",
                                    function_pointer_type->to_string(), actual_num_parameters, expected_num_parameters
                            )
                    );
                }
                assert(actual_num_parameters == expected_num_parameters and "how should this even go wrong?");
                for (usize i = 0; i < expected_num_parameters; ++i) {
                    const auto actual = expression.arguments[i]->data_type;
                    const auto expected = function_pointer_type->parameter_types[i];
                    if (*actual != *expected) {
                        Error::error(
                                *(expression.arguments[i]),
                                fmt::format(
                                        R"(argument type mismatch: got "{}", expected "{}")", actual->to_string(),
                                        expected->to_string()
                                )
                        );
                    }
                }
                expression.data_type = function_pointer_type->return_type;
                expression.value_type = ValueType::RValue;
                expression.function_to_call = FunctionPointerMarker{};
            } else {
                auto& possible_overloads = name->possible_overloads.value();
                const auto& name_token = name->name_tokens.back();
                const auto identifier = Error::token_location(name_token).view();

                using std::ranges::views::transform;
                const auto signature = fmt::format(
                        "{}({})", identifier,
                        fmt::join(
                                expression.arguments | transform([](const auto& argument) {
                                    return argument->data_type->to_string();
                                }),
                                ", "
                        )
                );
                bool overload_found = false;
                for (const auto& overload : possible_overloads) {
                    if (overload->signature == signature) {
                        assert(overload->definition->return_type != nullptr and "return type has to be set before");
                        for ([[maybe_unused]] const auto& parameter : overload->definition->parameters) {
                            assert(parameter.data_type != nullptr and "parameter type has to be set before");
                        }

                        using std::ranges::views::transform;
                        const auto range = overload->definition->parameters
                                           | transform([](const auto& parameter) { return parameter.data_type; });
                        auto parameter_types = std::vector(
                                cbegin(range), cend(range)
                        ); // no curly braces because of constructor ambiguity

                        name->data_type = type_container->from_type_definition(std::make_unique<FunctionPointerType>(
                                std::move(parameter_types), overload->definition->return_type
                        ));
                        overload_found = true;
                    }
                }
                if (not overload_found) {
                    Error::error(
                            name_token,
                            fmt::format("no matching function overload found with signature \"{}\"", signature)
                    );
                }
                // erase all possible overloads with the wrong signature
                possible_overloads.erase(
                        std::remove_if(
                                std::begin(possible_overloads), std::end(possible_overloads),
                                [&](const auto& overload) { return overload->signature != signature; }
                        ),
                        std::end(possible_overloads)
                );
                // erase all overloads except for the first one (which is the "inner" one)
                possible_overloads.erase(std::begin(possible_overloads) + 1, std::end(possible_overloads));
                assert(possible_overloads.size() == 1);
                expression.data_type = possible_overloads.front()->definition->return_type;
                expression.value_type = ValueType::RValue;
                assert(possible_overloads.front()->definition and "each overload must have a pointer to its definition"
                );
                expression.function_to_call = possible_overloads.front()->definition;
            }
        }

        void visit(Parser::Expressions::Assignment& expression) override {
            expression.assignee->accept(*this);
            expression.value->accept(*this);

            // check if the left side is an lvalue
            if (not expression.assignee->is_lvalue()) {
                Error::error(expression.equals_token, "left-hand side of assignment must be an lvalue");
            }

            // check if the left side is mutable
            if (expression.assignee->value_type != ValueType::MutableLValue) {
                Error::error(expression.equals_token, "cannot assign to expression since assignee is immutable");
            }

            // The right side must be an rvalue
            expression.value->value_type = ValueType::RValue;

            const auto resulting_data_type = get_resulting_data_type(
                    expression.assignee->data_type, expression.equals_token, expression.value->data_type,
                    *type_container
            );
            if (resulting_data_type != nullptr) {
                expression.data_type = resulting_data_type;
                expression.value_type = ValueType::RValue;
            } else {
                Error::error(
                        expression.equals_token,
                        fmt::format(
                                R"(expression of type "{}" cannot be assigned to expression of type "{}")",
                                expression.value->data_type->to_string(), expression.assignee->data_type->to_string()
                        )
                );
            }
        }

        void visit(Parser::Expressions::Nothing& expression) override {
            expression.data_type = type_container->get_nothing();
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::TypeSizeExpression& expression) override {
            const auto placeholder_type =
                    get_data_type_of_placeholder(type_container, expression.type_definition.get());
            if (placeholder_type != nullptr) {
                expression.contained_data_type = placeholder_type;
            } else {
                if (not type_container->is_defined(*(expression.type_definition))) {
                    Error::error(
                            expression.contained_data_type_tokens.front(),
                            fmt::format("use of undeclared type \"{}\"", expression.type_definition->to_string())
                    );
                }
                expression.contained_data_type =
                        type_container->from_type_definition(std::move(expression.type_definition));
            }
            expression.data_type = type_container->get_u32();
            expression.value_type = ValueType::RValue;
        }

        void visit(Parser::Expressions::ValueSizeExpression& expression) override {
            expression.expression->accept(*this);
            expression.data_type = type_container->get_u32();
            expression.value_type = ValueType::RValue;
        }

        TypeContainer* type_container;
        const Parser::FunctionDefinition* surrounding_function;
    };

    static void
    visit_top_level_statements(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope);

    struct TypeCheckerTopLevelVisitor {
        TypeCheckerTopLevelVisitor(Parser::Program* program, TypeContainer* type_container, const Scope* global_scope)
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        void operator()(std::unique_ptr<Parser::CustomTypeDefinition>&) { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            // the actual signature of the function must have been visited before, we now
            // only visit the body
            auto visitor = TypeCheckerVisitor{ type_container, function_definition.get() };
            function_definition->body.accept(visitor);
        }

        void operator()(std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
            visit_top_level_statements(namespace_definition->contents, *type_container, *global_scope);
        }

        Parser::Program* program;
        TypeContainer* type_container;
        const Scope* global_scope;
    };

    static void
    visit_function_definitions(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope);
    static void visit_custom_type_definitions(Parser::Program& program, TypeContainer& type_container);

    struct CustomTypeDefinitionVisitor {
        explicit CustomTypeDefinitionVisitor(TypeContainer* type_container) : type_container{ type_container } { }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        [[nodiscard]] bool has_cyclic_dependency(
                const Parser::VariantDefinition* struct_definition,
                std::unordered_set<const Parser::VariantDefinition*>& visited_struct_definitions
        ) const {
            // first put the current struct definition into the set -- if its already there, we have found a cycle
            if (visited_struct_definitions.contains(struct_definition)) {
                return true;
            }
            visited_struct_definitions.insert(struct_definition);

            // now check all attribute types of the struct and "follow the link" if they contain structs themselves
            for (const auto& attribute : struct_definition->members) {
                if (attribute.type_definition->is_custom_type_placeholder()) {
                    const auto placeholder_type = *(attribute.type_definition->as_custom_type_placeholder());
                    if (placeholder_type->struct_definition != nullptr) {
                        if (has_cyclic_dependency(placeholder_type->struct_definition, visited_struct_definitions)) {
                            return true;
                        }
                    } else if (placeholder_type->custom_type_definition != nullptr) {
                        assert(false and "not implemented");
                    } else {
                        assert(false and "unreachable");
                    }
                }
            }
            visited_struct_definitions.erase(struct_definition);
            return false;
        }

        [[nodiscard]] bool contains_placeholder(const DataType* type_definition) const {
            assert(type_definition != nullptr);
            if (type_definition->is_primitive_type()) {
                return false;
            }
            if (type_definition->is_custom_type_placeholder()) {
                return true;
            }
            if (type_definition->is_pointer_type()) {
                return contains_placeholder((*(type_definition->as_pointer_type()))->contained);
            }
            if (type_definition->is_array_type()) {
                return contains_placeholder((*(type_definition->as_array_type()))->contained);
            }
            if (type_definition->is_function_pointer_type()) {
                const auto function_pointer_type = *(type_definition->as_function_pointer_type());
                for (const auto parameter_type : function_pointer_type->parameter_types) {
                    if (contains_placeholder(parameter_type)) {
                        return true;
                    }
                }
                return contains_placeholder(function_pointer_type->return_type);
            }
            assert(false and "unreachable");
            return false;
        }

        /**
         * This function makes a type known to the type system, that contains a placeholder. Example: A type like
         * ->S (containing the placeholder for S) will result in an existing data type ("existing" meaning that its
         * present in the type container) that corresponds to a pointer to a placeholder to S.
         * Important: This type is not ready to be used in later stages of the compilation, since placeholder types
         * have to be replaced by their "real" counterparts.
         *
         * @param type_definition The definition of a type that contains a placeholder type.
         * @return A pointer to the corresponding type that is now known to the type container.
         */
        [[nodiscard]] DataType* data_type_from_definition_with_placeholder(const DataType* type_definition) const {
            // only placeholders for structs and custom types are allowed here
            assert(not type_definition->is_struct_type());
            assert(not type_definition->is_custom_type());

            if (type_definition->is_primitive_type()) {
                const auto primitive_type_definition = *(type_definition->as_primitive_type());
                return type_container->from_type_definition(
                        std::make_unique<PrimitiveType>(primitive_type_definition->type)
                );
            }
            if (type_definition->is_custom_type_placeholder()) {
                const auto placeholder_definition = *(type_definition->as_custom_type_placeholder());
                auto result = std::make_unique<CustomTypePlaceholder>(placeholder_definition->type_definition_tokens);
                result->struct_definition = placeholder_definition->struct_definition;
                result->custom_type_definition = placeholder_definition->custom_type_definition;
                return type_container->from_type_definition(std::move(result));
            }
            if (type_definition->is_pointer_type()) {
                const auto pointer_type_definition = *(type_definition->as_pointer_type());
                return type_container->pointer_to(
                        data_type_from_definition_with_placeholder(pointer_type_definition->contained),
                        pointer_type_definition->binding_mutability
                );
            }
            if (type_definition->is_array_type()) {
                const auto array_type_definition = *(type_definition->as_array_type());
                return type_container->array_of(
                        data_type_from_definition_with_placeholder(array_type_definition->contained),
                        array_type_definition->num_elements
                );
            }
            if (type_definition->is_function_pointer_type()) {
                const auto function_pointer_type_definition = *(type_definition->as_function_pointer_type());
                auto parameter_types = std::vector<DataType*>{};
                parameter_types.reserve(function_pointer_type_definition->parameter_types.size());
                for (const auto& parameter : function_pointer_type_definition->parameter_types) {
                    parameter_types.push_back(data_type_from_definition_with_placeholder(parameter));
                }
                const auto return_type =
                        data_type_from_definition_with_placeholder(function_pointer_type_definition->return_type);
                return type_container->function_pointer(std::move(parameter_types), return_type);
            }
            assert(false and "unreachable");
            return nullptr;
        }

        void operator()(std::unique_ptr<Parser::CustomTypeDefinition>& type_definition) {
            assert(not type_definition->alternatives.empty());
            assert(type_definition->name.has_value());

            const auto namespace_qualifier =
                    get_absolute_namespace_qualifier(*(type_definition->surrounding_scope->surrounding_namespace));

            auto struct_types = std::vector<const StructType*>{};
            struct_types.reserve(type_definition->alternatives.size());

            // first check for dependency cycles
            for (auto& [tag, struct_definition] : type_definition->alternatives) {
                auto visited_struct_definitions = std::unordered_set<const Parser::VariantDefinition*>{};
                const auto cycle_detected = has_cyclic_dependency(&struct_definition, visited_struct_definitions);
                if (cycle_detected) {
                    Error::error(struct_definition.name, "cyclic type definition detected");
                }
            }

            // iterate variants
            for (auto& [tag, struct_definition] : type_definition->alternatives) {
                auto member_types = std::vector<StructMember>{};
                member_types.reserve(struct_definition.members.size());

                // iterate attributes of current struct
                for (auto& attribute : struct_definition.members) {
                    /* We do not check the types of the attributes now, because we first have to make
                     * all types known to the type system, since one struct type could contain a member of
                     * another struct type, that has not been seen yet */

                    const auto has_placeholder_inside = contains_placeholder(attribute.type_definition.get());
                    if (not has_placeholder_inside) {
                        if (not type_container->is_defined(*(attribute.type_definition))) {
                            // TODO: improve error message by pointing to the data type instead of the name
                            Error::error(
                                    attribute.name,
                                    fmt::format("use of undeclared type \"{}\"", attribute.type_definition->to_string())
                            );
                        }
                        attribute.type = type_container->from_type_definition(std::move(attribute.type_definition));
                    } else {
                        attribute.type = data_type_from_definition_with_placeholder(attribute.type_definition.get());
                    }
                    member_types.push_back(StructMember{ .name{ std::string{ attribute.name.location.view() } },
                                                         .data_type{ attribute.type },
                                                         .offset{} });
                }

                const auto struct_data_type = type_container->from_type_definition(std::make_unique<StructType>(
                        std::string{ struct_definition.name.location.view() }, namespace_qualifier,
                        std::string{ (*(type_definition->name)).location.view() }, std::move(member_types)
                ));
                struct_definition.data_type = struct_data_type;

                const auto struct_type = dynamic_cast<const StructType*>(struct_data_type);
                assert(struct_type != nullptr);
                struct_types.push_back(struct_type);
            }

            const auto custom_data_type = type_container->from_type_definition(std::make_unique<CustomType>(
                    std::string{ type_definition->name->location.view() }, namespace_qualifier, std::move(struct_types)
            ));
            type_definition->data_type = custom_data_type;
        }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>&) { }

        void operator()(std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
            visit_custom_type_definitions(namespace_definition->contents, *type_container);
        }

        TypeContainer* type_container;
    };

    struct FunctionDefinitionVisitor {
        FunctionDefinitionVisitor(TypeContainer* type_container, const Scope* global_scope)
            : type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        void operator()(std::unique_ptr<Parser::CustomTypeDefinition>&) { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using std::ranges::find_if, std::ranges::views::transform;

            for (auto& parameter : function_definition->parameters) {
                assert(parameter.type_definition and "type definition must have been set before");

                const auto placeholder_type =
                        get_data_type_of_placeholder(type_container, parameter.type_definition.get());
                if (placeholder_type != nullptr) {
                    parameter.data_type = placeholder_type;
                } else {
                    if (not type_container->is_defined(*(parameter.type_definition))) {
                        Error::error(
                                parameter.name,
                                fmt::format("use of undeclared type \"{}\"", parameter.type_definition->to_string())
                        );
                    }
                    parameter.data_type = type_container->from_type_definition(std::move(parameter.type_definition));
                }
            }

            auto signature = fmt::format(
                    "{}({})", function_definition->name.location.view(),
                    fmt::join(
                            function_definition->parameters
                                    | transform([](const auto& parameter) { return parameter.data_type->to_string(); }),
                            ", "
                    )
            );

            const auto identifier = function_definition->name.location.view();

            const auto& scope = function_definition->surrounding_scope;
            const auto find_iterator = scope->find(identifier);
            [[maybe_unused]] const auto found = (find_iterator != std::cend(*scope));
            assert(found && "scope generator should have put the needed symbol into scope or throw an error");
            const auto& found_symbol = find_iterator->second;
            assert(std::holds_alternative<FunctionSymbol>(found_symbol));
            const auto& function_symbol = std::get<FunctionSymbol>(found_symbol);
            const auto& overloads = function_symbol.overloads;

            for (const auto& overload : overloads) {
                const auto duplicate_signature = (overload.signature == signature);
                if (duplicate_signature) {
                    assert(overload.surrounding_namespace
                                   == function_definition->corresponding_symbol->surrounding_namespace
                           and "functions in the same scope must be in the same namespace");

                    Error::error(
                            function_definition->name,
                            fmt::format("function overload for \"{}\" with ambiguous signature", identifier)
                    );
                }
            }

            function_definition->corresponding_symbol->signature = std::move(signature);

            assert(function_definition->return_type_definition and "function return type must have been set before");

            const auto placeholder_type =
                    get_data_type_of_placeholder(type_container, function_definition->return_type_definition.get());
            if (placeholder_type != nullptr) {
                function_definition->return_type = placeholder_type;
            } else {
                if (not type_container->is_defined(*(function_definition->return_type_definition))) {
                    assert(not function_definition->return_type_definition_tokens.empty()
                           and "parser must have set this value");
                    Error::error(
                            function_definition->return_type_definition_tokens.front(),
                            fmt::format(
                                    "use of undeclared type \"{}\"",
                                    function_definition->return_type_definition->to_string()
                            )
                    );
                }
                function_definition->return_type =
                        type_container->from_type_definition(std::move(function_definition->return_type_definition));
            }
            function_definition->corresponding_symbol->definition->return_type = function_definition->return_type;

            // the body of the function is not recursively visited here since we have to first visit
            // all function signatures before visiting the bodies
        }

        void operator()(std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
            visit_function_definitions(namespace_definition->contents, *type_container, *global_scope);
        }

        TypeContainer* type_container;
        const Scope* global_scope;
    };

    static void replace_placeholder_types(Parser::Program& program, TypeContainer& type_container);

    /** Visits all custom type definitions and replaces placeholder types within them
     * */
    struct PlaceholderReplacementVisitor {
        explicit PlaceholderReplacementVisitor(TypeContainer* type_container) : type_container{ type_container } { }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        void operator()(std::unique_ptr<Parser::CustomTypeDefinition>& type_definition) {
            for (auto& [tag, struct_definition] : type_definition->alternatives) {
                {
                    const auto placeholder_type =
                            get_data_type_of_placeholder(type_container, struct_definition.data_type);
                    if (placeholder_type != nullptr) {
                        struct_definition.data_type = placeholder_type;
                    }
                }
                for (auto& attribute : struct_definition.members) {
                    const auto placeholder_type = get_data_type_of_placeholder(type_container, attribute.type);
                    if (placeholder_type != nullptr) {
                        attribute.type = placeholder_type;
                    }
                }
            }
        }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>&) { }

        void operator()(std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
            replace_placeholder_types(namespace_definition->contents, *type_container);
        }

        TypeContainer* type_container;
    };

    static void visit_custom_type_definitions(Parser::Program& program, TypeContainer& type_container) {
        auto visitor = CustomTypeDefinitionVisitor{ &type_container };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    static void
    visit_function_definitions(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        auto visitor = FunctionDefinitionVisitor{ &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    static void
    visit_top_level_statements(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        auto visitor = TypeCheckerTopLevelVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    static void check_return_statements(Parser::Program& program, TypeContainer& type_container) {
        for (auto& top_level_statement : program) {
            std::visit(
                    overloaded{ [&](const std::unique_ptr<Parser::FunctionDefinition>& function) {
                                   auto return_type_checker = ReturnTypeChecker{};
                                   function->body.accept(return_type_checker);
                                   if (function->return_type != type_container.get_nothing()
                                       and not return_type_checker.all_code_paths_return_a_value) {
                                       Error::error(function->name, "not all code paths return a value\n");
                                   }
                               },
                                [&](const std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
                                    check_return_statements(namespace_definition->contents, type_container);
                                },
                                [](const auto&) {} },
                    top_level_statement
            );
        }
    }

    static void replace_placeholder_types(Parser::Program& program, TypeContainer& type_container) {
        auto visitor = PlaceholderReplacementVisitor{ &type_container };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    void check(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        // first we have to look at all type definitions to make them available to the rest of the code
        visit_custom_type_definitions(program, type_container);

        replace_placeholder_types(program, type_container);

        // then we check the types of the actual function definitions
        visit_function_definitions(program, type_container, global_scope);

        // then we have to check the bodies of the functions
        visit_top_level_statements(program, type_container, global_scope);

        // For functions that return a value (different from nothing) we have to check if all code paths
        // actually do return a value. We run the check for all functions, though, because it also throws a warning
        // on unreachable code (which could be interesting for functions that return nothing).
        check_return_statements(program, type_container);
    }
} // namespace TypeChecker
