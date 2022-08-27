//
// Created by coder2k on 10.07.2022.
//

#include "type_checker.hpp"
#include "error.hpp"
#include "return_type_checker.hpp"
#include <algorithm>
#include <array>
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <limits>
#include <ranges>
#include <string_view>
#include <variant>

[[nodiscard]] u8 char_to_digit(char c, const u64 base) {
    constexpr auto valid_digits =
            std::array{ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };
    assert(base <= 16);
    c = static_cast<char>(std::toupper(c));
    for (usize i = 0; i < base; ++i) {
        if (c == valid_digits[i]) {
            if (i < 10) {
                return static_cast<u8>(c - '0');
            } else {
                return static_cast<u8>(c - 'A' + 10);
            }
        }
    }
    assert(false and "invalid input character");
    return 0;
}

[[nodiscard]] bool validate_integer(const std::string_view text, const u64 base) {
    u64 result = 0;
    u64 factor = 1;
    for (char c : std::ranges::reverse_view(text)) {
        const auto digit = char_to_digit(c, base);
        result += digit * factor;
        factor *= base;
        if (result > std::numeric_limits<u32>::max()) {
            return false;
        }
    }
    return true;
}

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

    [[nodiscard]] std::optional<std::string_view> concrete_type(const DataType* data_type) {
        if (const auto concrete = dynamic_cast<const ConcreteType*>(data_type)) {
            return concrete->name;
        }
        return {};
    }

    [[nodiscard]] const DataType* get_resulting_data_type_for_function_pointers(
            const FunctionPointerType* lhs,
            const Token& token,
            const FunctionPointerType* rhs,
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

    [[nodiscard]] const DataType*
    get_resulting_data_type_for_pointers(const PointerType* lhs, const Token& token, const PointerType* rhs) {
        using namespace Lexer::Tokens;
        if (lhs->contained != rhs->contained) {
            // pointers point to different data types
            return nullptr;
        }
        if (is<Equals>(token)) {
            return lhs;
        }
        return nullptr;
    }

    [[nodiscard]] const DataType* get_resulting_data_type(
            const DataType* lhs,
            const Token& token,
            const DataType* rhs,
            TypeContainer& type_container
    ) {
        using namespace Lexer::Tokens;
        assert(lhs != nullptr);
        assert(rhs != nullptr);

        const auto first_function_pointer = dynamic_cast<const FunctionPointerType*>(lhs); // maybe nullptr
        const auto second_function_pointer = dynamic_cast<const FunctionPointerType*>(rhs);// maybe nullptr

        if (first_function_pointer != nullptr and second_function_pointer != nullptr) {
            return get_resulting_data_type_for_function_pointers(
                    first_function_pointer, token, second_function_pointer, type_container
            );
        } else if (first_function_pointer != nullptr or second_function_pointer != nullptr) {
            return nullptr;
        }

        // if neither operand is a function pointer, we evaluate the actual types

        const auto same_type = (lhs == rhs);

        const auto first_pointer = dynamic_cast<const PointerType*>(lhs); // maybe nullptr
        const auto second_pointer = dynamic_cast<const PointerType*>(lhs);// maybe nullptr
        if (first_pointer != nullptr and second_pointer != nullptr) {
            return get_resulting_data_type_for_pointers(first_pointer, token, second_pointer);
        } else if (first_pointer != nullptr or second_pointer != nullptr) {
            return nullptr;
        }

        // the following array represents the concrete data types ignoring their mutability
        const auto concrete_types = std::array{ concrete_type(lhs), concrete_type(rhs) };
        const auto both_concrete = (concrete_types[0].has_value() and concrete_types[1].has_value());
        if (is_one_of<EqualsEquals, ExclamationEquals>(token)) {
            if (not both_concrete or concrete_types[0].value() != concrete_types[1].value()) {
                return nullptr;
            }
            return type_container.get_bool();
        }
        if (is_one_of<GreaterThan, GreaterOrEquals, LessThan, LessOrEquals>(token)) {
            if (not both_concrete or concrete_types[0].value() != concrete_types[1].value() or
                concrete_types[0].value() != U32Identifier) {
                return nullptr;
            }
            return type_container.get_bool();
        }
        if (is<Equals>(token)) {
            if (not both_concrete or concrete_types[0].value() != concrete_types[1].value()) {
                return nullptr;
            }
            return rhs;
        }
        if (is_one_of<Plus, Minus, Asterisk, ForwardSlash, Mod>(token)) {
            if (not both_concrete or concrete_types[0].value() != concrete_types[1].value()) {
                return nullptr;
            }
            return concrete_types[0].value() == U32Identifier ? type_container.get_u32() : nullptr;
        }
        if (is_one_of<And, Or, Xor>(token)) {
            if (not same_type or not both_concrete) {
                return nullptr;
            }
            return concrete_types[0].value() == BoolIdentifier ? lhs : nullptr;
        }
        return nullptr;
    }

    [[nodiscard]] const DataType*
    get_resulting_data_type(const Parser::Expressions::BinaryOperator& expression, TypeContainer& type_container) {
        return get_resulting_data_type(
                expression.lhs->data_type, expression.operator_token, expression.rhs->data_type, type_container
        );
    }

    template<typename... Types>
    [[nodiscard]] static bool holds_any_of(const Lexer::Tokens::Token& token) {
        return (std::holds_alternative<Types>(token) or ...);
    }

    struct TypeCheckerVisitor : public Parser::Statements::StatementVisitor,
                                public Parser::Expressions::ExpressionVisitor {
        TypeCheckerVisitor(
                TypeContainer* type_container,
                usize starting_offset,
                const Parser::FunctionDefinition* surrounding_function
        )
            : type_container{ type_container },
              surrounding_function{ surrounding_function } {
            if (starting_offset > 0) {
                claim_stack_space(starting_offset);
            }
        }

        void visit(Parser::Statements::Block& statement) override {
            const auto old_offset = offset;
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
            statement.occupied_stack_space = occupied_stack_space;
            offset = old_offset;
        }

        void visit(Parser::Statements::IfStatement& statement) override {
            statement.condition->accept(*this);
            // condition must be an rvalue
            statement.condition->value_type = ValueType::RValue;

            const auto condition_type = dynamic_cast<const ConcreteType*>(statement.condition->data_type);
            if (condition_type == nullptr or condition_type->name != BoolIdentifier) {
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

            const auto condition_type = dynamic_cast<const ConcreteType*>(statement.condition->data_type);
            if (condition_type == nullptr or condition_type->name != BoolIdentifier) {
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

            const auto condition_type = dynamic_cast<const ConcreteType*>(statement.condition->data_type);
            if (condition_type == nullptr or condition_type->name != BoolIdentifier) {
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
            const auto old_offset = offset;
            if (statement.initializer) {
                statement.initializer->accept(*this);
            }
            if (statement.condition) {
                statement.condition->accept(*this);
                // condition must be an rvalue
                statement.condition->value_type = ValueType::RValue;

                const auto condition_type = dynamic_cast<const ConcreteType*>(statement.condition->data_type);
                if (condition_type == nullptr or condition_type->name != BoolIdentifier) {
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
            offset = old_offset;
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
            assert(statement.type_definition and "type definition must have been set before");// TODO: type deduction
            auto& type = statement.type_definition;
            if (not type_container->is_defined(*type)) {
                Error::error(statement.name, fmt::format("use of undeclared type \"{}\"", type->to_string()));
            }
            statement.type = type_container->from_type_definition(std::move(statement.type_definition));

            assert(statement.variable_symbol != nullptr);
            statement.variable_symbol->offset = claim_stack_space(statement.type->size());

            statement.initial_value->accept(*this);
            // initial value must be an rvalue
            statement.initial_value->value_type = ValueType::RValue;

            assert(statement.type and statement.initial_value->data_type and "missing type information");

            // get the mutable version of the assignee type (if it is not mutable already)
            DataType const* const assignee_type = statement.type;

            // type checking rules are the same as if we would do type checking during an assignment
            const auto resulting_type = get_resulting_data_type(
                    assignee_type, statement.equals_token, statement.initial_value->data_type, *type_container
            );

            if (resulting_type == nullptr) {
                Error::error(
                        statement.equals_token,
                        fmt::format(
                                R"(cannot initialize a variable of type "{}" with a value of type "{}")",
                                statement.type->to_string(), statement.initial_value->data_type->to_string()
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
            auto number_string = std::string{};
            for (const auto c : expression.value.location.view()) {
                if (c != '_') {
                    number_string += c;
                }
            }
            u64 base = 10;
            if (number_string.starts_with("0x")) {
                base = 16;
            } else if (number_string.starts_with("0o")) {
                base = 8;
            } else if (number_string.starts_with("0b")) {
                base = 2;
            }
            const auto digits_view =
                    (base == 10 ? std::string_view{ number_string }
                                : std::string_view{ number_string.begin() + 2, number_string.end() });
            if (not validate_integer(digits_view, base)) {
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

        void visit(Parser::Expressions::Name& expression) override {
            using Parser::Statements::VariableDefinition;
            const auto is_variable = expression.variable_symbol.has_value();

            if (is_variable) {
                // now we visit the definition that is the origin of this variable
                const auto& definition = expression.variable_symbol.value()->definition;
                expression.data_type = std::visit(
                        overloaded{ [&](const VariableDefinition* variable_definition) -> const DataType* {
                                       assert(variable_definition->type and "type must be known");
                                       expression.value_type =
                                               (variable_definition->binding_mutability == Mutability::Mutable
                                                        ? ValueType::MutableLValue
                                                        : ValueType::ConstLValue);
                                       return variable_definition->type;
                                   },
                                    [&](const Parameter* parameter_definition) -> const DataType* {
                                        assert(parameter_definition->type and "type must be known");
                                        expression.value_type =
                                                (parameter_definition->binding_mutability == Mutability::Mutable
                                                         ? ValueType::MutableLValue
                                                         : ValueType::ConstLValue);
                                        return parameter_definition->type;
                                    },
                                    [](std::monostate) -> const DataType* {
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
                    const auto& overloads = *expression.possible_overloads;//function_symbol->overloads;
                    assert(not overloads.empty() and "there shall never be a function with zero overloads");
                    if (overloads.size() > 1) {
                        Error::error(name_token, fmt::format("use of identifier \"{}\" is ambiguous", identifier));
                    }
                    assert(overloads.size() == 1);
                    const auto function_definition = overloads.front()->definition;
                    auto parameter_types = std::vector<const DataType*>{};
                    parameter_types.reserve(function_definition->parameters.size());
                    for (const auto& parameter : function_definition->parameters) {
                        parameter_types.push_back(parameter.type);
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
            } else {
                assert(false and "not implemented");
            }
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            using namespace Lexer::Tokens;
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);

            // both operands are required to be rvalues
            expression.lhs->value_type = ValueType::RValue;
            expression.rhs->value_type = ValueType::RValue;

            if (const auto resulting_type = get_resulting_data_type(expression, *type_container)) {
                expression.data_type = resulting_type;
                expression.value_type = ValueType::RValue;
                return;
            }
            Error::error(
                    expression.operator_token,
                    fmt::format(
                            R"(operator "{}" can not be applied to operands of type "{}" and "{}")",
                            Error::token_location(expression.operator_token).view(),
                            expression.lhs->data_type->to_string(), expression.rhs->data_type->to_string()
                    )
            );
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
                        for (const auto& parameter : overload->definition->parameters) {
                            assert(parameter.type != nullptr and "parameter type has to be set before");
                        }

                        using std::ranges::views::transform;
                        const auto range = overload->definition->parameters |
                                           transform([](const auto& parameter) { return parameter.type; });
                        auto parameter_types = std::vector(
                                cbegin(range), cend(range)
                        );// no curly braces because of constructor ambiguity

                        name->data_type = type_container->from_type_definition(std::make_unique<FunctionPointerType>(
                                std::move(parameter_types), overload->definition->return_type
                        ));
                        overload_found = true;
                    }
                }
                if (not overload_found) {
                    Error::error(name_token, fmt::format("no matching function overload found", identifier));
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

        usize claim_stack_space(const usize size_of_type) {
            const auto old_offset = offset;
            offset += size_of_type;
            if (offset > occupied_stack_space) {
                occupied_stack_space = offset;
            }
            return old_offset;
        }

        TypeContainer* type_container;
        usize offset{ 0 };
        usize occupied_stack_space{ 0 };
        const Parser::FunctionDefinition* surrounding_function;
    };

    struct TypeCheckerTopLevelVisitor {
        TypeCheckerTopLevelVisitor(
                const Parser::Program* program,
                TypeContainer* type_container,
                const Scope* global_scope
        )
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            // the actual signature of the function must have been visited before, we now
            // only visit the body
            usize size_of_parameters = 0;
            for (const auto& parameter : function_definition->parameters) {
                size_of_parameters += parameter.type->size();
            }
            auto visitor = TypeCheckerVisitor{ type_container, size_of_parameters, function_definition.get() };
            function_definition->body.accept(visitor);
            function_definition->occupied_stack_space = visitor.occupied_stack_space;
            function_definition->parameters_stack_space = size_of_parameters;
        }

        void operator()(std::unique_ptr<Parser::ImportStatement>&) { }

        const Parser::Program* program;
        TypeContainer* type_container;
        const Scope* global_scope;
    };

    struct FunctionDefinitionVisitor {
        FunctionDefinitionVisitor(TypeContainer* type_container, const Scope* global_scope)
            : type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using std::ranges::find_if, std::ranges::views::transform;
            usize offset = 0;

            for (auto& parameter : function_definition->parameters) {
                assert(parameter.type_definition and "type definition must have been set before");
                if (not type_container->is_defined(*(parameter.type_definition))) {
                    Error::error(
                            parameter.name,
                            fmt::format("use of undeclared type \"{}\"", parameter.type_definition->to_string())
                    );
                }
                parameter.type = type_container->from_type_definition(std::move(parameter.type_definition));
                parameter.variable_symbol->offset = offset;
                offset += parameter.type->size();
            }

            auto signature = fmt::format(
                    "{}({})", function_definition->name.location.view(),
                    fmt::join(
                            function_definition->parameters |
                                    transform([](const auto& parameter) { return parameter.type->to_string(); }),
                            ", "
                    )
            );

            const auto identifier = function_definition->name.location.view();

            const auto find_iterator = global_scope->find(identifier);
            const auto found = (find_iterator != std::cend(*global_scope));
            assert(found && "scope generator should have put the needed symbol into scope or throw an error");
            const auto& found_symbol = find_iterator->second;
            assert(std::holds_alternative<FunctionSymbol>(found_symbol));
            const auto& function_symbol = std::get<FunctionSymbol>(found_symbol);
            const auto& overloads = function_symbol.overloads;

            for (const auto& overload : overloads) {
                const auto duplicate_signature = (overload.signature == signature);
                if (duplicate_signature) {
                    const auto same_namespace = (overload.namespace_name == function_definition->namespace_name);
                    if (same_namespace) {
                        Error::error(
                                function_definition->name,
                                fmt::format("function overload for \"{}\" with ambiguous signature", identifier)
                        );
                    }
                }
            }

            function_definition->corresponding_symbol->signature = std::move(signature);

            assert(function_definition->return_type_definition and "function return type must have been set before");
            if (not type_container->is_defined(*(function_definition->return_type_definition))) {
                Error::error(
                        function_definition->name, fmt::format(
                                                           "use of undeclared type \"{}\"",
                                                           function_definition->return_type_definition->to_string()
                                                   )
                );
            }

            function_definition->return_type =
                    type_container->from_type_definition(std::move(function_definition->return_type_definition));
            function_definition->corresponding_symbol->definition->return_type = function_definition->return_type;

            // the body of the function is not recursively visited here since we have to first visit
            // all function signatures before visiting the bodies
        }

        void operator()(auto&) { }

        TypeContainer* type_container;
        const Scope* global_scope;
    };

    void
    visit_function_definitions(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        auto visitor = FunctionDefinitionVisitor{ &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

    void check(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope) {
        // first we check the types of the actual function definitions
        visit_function_definitions(program, type_container, global_scope);

        // then we have to check the bodies of the functions
        auto visitor = TypeCheckerTopLevelVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }

        // For functions that return a value (different from nothing) we have to check if all code paths
        // actually do return a value. We run the check for all functions, though, because it also throws a warning
        // on unreachable code (which could be interesting for functions that return nothing).
        for (auto& top_level_statement : program) {
            std::visit(
                    overloaded{ [&](const std::unique_ptr<Parser::FunctionDefinition>& function) {
                                   auto return_type_checker = ReturnTypeChecker{};
                                   function->body.accept(return_type_checker);
                                   if (function->return_type != type_container.get_nothing() and
                                       not return_type_checker.all_code_paths_return_a_value) {
                                       Error::error(function->name, "not all code paths return a value\n");
                                   }
                               },
                                [](const auto&) {} },
                    top_level_statement
            );
        }
    }
}// namespace TypeChecker
