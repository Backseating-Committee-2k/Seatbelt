//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "error.hpp"
#include "magic_enum_wrapper.hpp"
#include "mutability.hpp"
#include "type_container.hpp"
#include "types.hpp"
#include "utils.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

static inline constexpr std::string_view FunctionPointerKeyword{ "Function" };

struct PrimitiveType;
struct ArrayType;
struct StructType;
struct CustomType;
struct CustomTypePlaceholder;
struct PointerType;
struct FunctionPointerType;
namespace Parser {
    struct CustomTypeDefinition;
}

struct DataType {
public:
    virtual ~DataType() = default;

    [[nodiscard]] virtual bool operator==(const DataType& other) const = 0;
    [[nodiscard]] virtual std::string to_string() const = 0;
    [[nodiscard]] virtual usize size() const = 0;
    [[nodiscard]] virtual usize alignment() const = 0;
    [[nodiscard]] virtual usize size_when_pushed() const = 0;

    [[nodiscard]] virtual usize num_words() const {
        assert(size() % 4 == 0);
        return size() / 4;
    }

    [[nodiscard]] virtual bool is_primitive_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_pointer_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_function_pointer_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_array_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_struct_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_custom_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_custom_type_placeholder() const {
        return false;
    }

    [[nodiscard]] virtual std::optional<const PrimitiveType*> as_primitive_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const ArrayType*> as_array_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const StructType*> as_struct_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<StructType*> as_struct_type() {
        return {};
    }

    [[nodiscard]] virtual std::optional<const CustomType*> as_custom_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const CustomTypePlaceholder*> as_custom_type_placeholder() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<CustomTypePlaceholder*> as_custom_type_placeholder() {
        return {};
    }

    [[nodiscard]] virtual std::optional<const PointerType*> as_pointer_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const FunctionPointerType*> as_function_pointer_type() const {
        return {};
    }

    [[nodiscard]] virtual bool contains_placeholders() const = 0;
};

enum class BasicType {
    U32,
    Char,
    Bool,
    Nothing,
};

struct PrimitiveType final : public DataType {
    explicit PrimitiveType(BasicType type) : type{ type } { }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const PrimitiveType*>(&other)) {
            return type == other_pointer->type;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return std::string{ magic_enum::enum_name(type) };
    }

    [[nodiscard]] usize size() const override {
        switch (type) {
            case BasicType::U32:
                return WordSize;
            case BasicType::Char:
            case BasicType::Bool:
                return 1;
            case BasicType::Nothing:
                return 0;
        }
        assert(false and "unreachable");
        return 0;
    }

    [[nodiscard]] usize alignment() const override {
        switch (type) {
            case BasicType::U32:
                return WordSize;
            case BasicType::Char:
            case BasicType::Bool:
            case BasicType::Nothing:
                return 1;
        }
        assert(false and "unreachable");
        return 0;
    }

    [[nodiscard]] usize size_when_pushed() const override {
        switch (type) {
            case BasicType::U32:
            case BasicType::Char:
            case BasicType::Bool:
                return WordSize;
            case BasicType::Nothing:
                return 0;
        }
        assert(false and "unreachable");
        return 0;
    }

    [[nodiscard]] bool is_primitive_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const PrimitiveType*> as_primitive_type() const override {
        return this;
    }

    [[nodiscard]] bool contains_placeholders() const override {
        return false;
    }

    BasicType type;
};

struct ArrayType final : public DataType {
    ArrayType(DataType* contained, usize num_elements) : contained{ contained }, num_elements{ num_elements } { }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const ArrayType*>(&other)) {
            return num_elements == other_pointer->num_elements and contained == other_pointer->contained;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return fmt::format("[{}; {}]", contained->to_string(), num_elements);
    }

    [[nodiscard]] usize size() const override {
        return contained->size() * num_elements;
    }

    [[nodiscard]] usize alignment() const override {
        return contained->alignment();
    }

    [[nodiscard]] usize size_when_pushed() const override {
        return num_elements * contained->size_when_pushed();
    }

    [[nodiscard]] bool is_array_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const ArrayType*> as_array_type() const override {
        return this;
    }

    [[nodiscard]] bool contains_placeholders() const override {
        return contained->contains_placeholders();
    }

    DataType* contained;
    usize num_elements;
};

struct StructMember {
    std::string name;
    DataType* data_type;
    std::optional<usize> offset{};

    [[nodiscard]] bool operator==(const StructMember& other) const {
        return data_type == other.data_type and name == other.name;
    }
};

struct StructType final : public DataType {
    StructType(std::string name, std::string namespace_qualifier, std::vector<StructMember> members)
        : name{ std::move(name) },
          namespace_qualifier{ std::move(namespace_qualifier) },
          members{ std::move(members) } { }

    StructType(
            std::string name,
            std::string namespace_qualifier,
            std::vector<StructMember> members,
            const Parser::CustomTypeDefinition* owning_custom_type_definition
    )
        : name{ std::move(name) },
          namespace_qualifier{ std::move(namespace_qualifier) },
          members{ std::move(members) },
          owning_custom_type_definition{ owning_custom_type_definition } { }

    [[nodiscard]] bool is_struct_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const StructType*> as_struct_type() const override {
        return this;
    }

    [[nodiscard]] std::optional<StructType*> as_struct_type() override {
        return this;
    }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const StructType*>(&other)) {
            return name == other_pointer->name and namespace_qualifier == other_pointer->namespace_qualifier
                   and members == other_pointer->members;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return namespace_qualifier + name;
    }

    [[nodiscard]] usize size() const override;

    [[nodiscard]] usize alignment() const override;

    [[nodiscard]] usize size_when_pushed() const override;

    [[nodiscard]] usize size_when_pushed_without_padding() const {
        const auto starting_value = (contains_tag() ? WordSize : 0);
        const auto result = std::accumulate(
                members.cbegin(), members.cend(), starting_value,
                [](const usize sum, const auto& member) { return sum + member.data_type->size_when_pushed(); }
        );
        assert(result % WordSize == 0);
        return result;
    }

    [[nodiscard]] bool contains_tag() const;

    [[nodiscard]] std::optional<u32> tag() const;

    [[nodiscard]] bool contains_placeholders() const override;

    std::string name;
    std::string namespace_qualifier;
    std::vector<StructMember> members;
    const Parser::CustomTypeDefinition* owning_custom_type_definition{ nullptr };
};

struct CustomType final : public DataType {
    CustomType(std::string name, std::string namespace_qualifier, std::map<u32, StructType*> struct_types)
        : name{ std::move(name) },
          namespace_qualifier{ std::move(namespace_qualifier) },
          struct_types{ std::move(struct_types) } { }

    [[nodiscard]] bool is_custom_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const CustomType*> as_custom_type() const override {
        return this;
    }

    [[nodiscard]] bool contains_tag() const {
        assert(not struct_types.empty());
        return struct_types.size() != 1;
    }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        const auto other_pointer = dynamic_cast<const CustomType*>(&other);
        if (other_pointer == nullptr) {
            return false;
        }
        if (struct_types.size() != other_pointer->struct_types.size() or name != other_pointer->name
            or namespace_qualifier != other_pointer->namespace_qualifier) {
            return false;
        }
        for (auto left_iterator = struct_types.cbegin(), right_iterator = other_pointer->struct_types.cbegin();
             left_iterator != struct_types.cend(); ++left_iterator, ++right_iterator) {
            const auto& [left_tag, left_struct_type] = *left_iterator;
            const auto& [right_tag, right_struct_type] = *right_iterator;
            if (left_tag != right_tag or not left_struct_type->operator==(*right_struct_type)) {
                return false;
            }
        }

        return true;
    }

    [[nodiscard]] std::string to_string() const override {
        return namespace_qualifier + name;
    }

    [[nodiscard]] bool is_anonymous() const {
        return name.empty();
    }

    [[nodiscard]] usize size() const override {
        assert(not struct_types.empty());
        assert(not contains_placeholders());

        namespace ranges = std::ranges;

        const auto calculate_struct_size = [&](const StructType* struct_type) {
            const auto contains_tag = (is_anonymous() ? false : struct_types.size() > 1);
            usize result = (contains_tag ? WordSize : 0);
            for (const auto& member : struct_type->members) {
                assert(not member.data_type->contains_placeholders());
                result = Utils::round_up(result, member.data_type->alignment());
                result += member.data_type->size();
            }
            return Utils::round_up(result, alignment());
        };

        return ranges::max(struct_types | ranges::views::transform([&](const auto& pair) {
                               const auto& struct_type = pair.second;
                               return calculate_struct_size(struct_type);
                           }));
    }

    [[nodiscard]] usize alignment() const override {
        assert(not struct_types.empty());
        // this loop should get optimized away in release builds
        for ([[maybe_unused]] const auto& [tag, struct_type] : struct_types) {
            assert(struct_type->alignment() <= WordSize);
        }

        if (contains_tag()) {
            return WordSize;
        }
        const auto max_alignment_iterator =
                std::max_element(struct_types.cbegin(), struct_types.cend(), [](const auto& lhs, const auto& rhs) {
                    return lhs.second->alignment() < rhs.second->alignment();
                });
        const auto result = (*max_alignment_iterator).second->alignment();
        return result;
    }

    [[nodiscard]] usize size_when_pushed() const override {
        assert(not struct_types.empty());
        usize max_size = 0;
        for (const auto& pair : struct_types) {
            usize sum = (contains_tag() ? WordSize : 0);
            for (const auto& member : pair.second->members) {
                sum += member.data_type->size_when_pushed();
            }
            if (sum > max_size) {
                max_size = sum;
            }
        }
        return max_size;
    }

    [[nodiscard]] bool contains_placeholders() const override {
        for (const auto& [tag, struct_type] : struct_types) {
            if (struct_type->contains_placeholders()) {
                return true;
            }
        }
        return false;
    }

    std::string name;
    std::string namespace_qualifier;
    std::map<u32, StructType*> struct_types;
};

namespace Parser {
    struct StructDefinition;
    struct CustomTypeDefinition;
} // namespace Parser

struct CustomTypePlaceholder : public DataType {
    explicit CustomTypePlaceholder(std::span<const Lexer::Tokens::Token> type_definition_tokens)
        : type_definition_tokens{ type_definition_tokens } { }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        using Lexer::Tokens::Identifier;
        if (const auto other_pointer = dynamic_cast<const CustomTypePlaceholder*>(&other)) {
            const auto lookup_already_happened =
                    ((struct_definition != nullptr or custom_type_definition != nullptr)
                     and (other_pointer->struct_definition != nullptr
                          or other_pointer->custom_type_definition != nullptr));
            if (lookup_already_happened) {
                if (struct_definition != nullptr) {
                    return struct_definition == other_pointer->struct_definition;
                }
                if (custom_type_definition != nullptr) {
                    return custom_type_definition == other_pointer->custom_type_definition;
                }
                assert(false and "unreachable");
            } else {
                // lookup has not happened yet, therefore we only compare the types by looking at their names
                if (type_definition_tokens.size() != other_pointer->type_definition_tokens.size()) {
                    return false;
                }
                for (usize i = 0; i < type_definition_tokens.size(); ++i) {
                    if (Error::token_location(type_definition_tokens[i]).view()
                        != Error::token_location(other_pointer->type_definition_tokens[i]).view()) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        using std::ranges::views::transform;
        return fmt::format(
                "Placeholder({})", fmt::join(
                                           type_definition_tokens | transform([](const auto& token) {
                                               return Error::token_location(token).view();
                                           }),
                                           ""
                                   )
        );
    }

    [[nodiscard]] usize size() const override {
        assert(false and "this type should not be used like this!");
        return 0;
    }

    [[nodiscard]] usize alignment() const override {
        assert(false and "this type should not be used like this!");
        return 0;
    }

    [[nodiscard]] usize size_when_pushed() const override {
        assert(false and "this type should not be used like this!");
        return 0;
    }

    [[nodiscard]] bool is_custom_type_placeholder() const override {
        return true;
    }

    [[nodiscard]] std::optional<const CustomTypePlaceholder*> as_custom_type_placeholder() const override {
        return this;
    }

    [[nodiscard]] std::optional<CustomTypePlaceholder*> as_custom_type_placeholder() override {
        return this;
    }

    [[nodiscard]] bool contains_placeholders() const override {
        return true;
    }

    std::span<const Lexer::Tokens::Token> type_definition_tokens;
    const Parser::StructDefinition* struct_definition{ nullptr };
    const Parser::CustomTypeDefinition* custom_type_definition{ nullptr };
};

struct PointerType final : public DataType {
    PointerType(DataType* contained, Mutability binding_mutability)
        : contained{ contained },
          binding_mutability{ binding_mutability } { }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const PointerType*>(&other)) {
            return binding_mutability == other_pointer->binding_mutability and *contained == *other_pointer->contained;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return fmt::format(
                "->{} {}", binding_mutability == Mutability::Mutable ? "mutable" : "const", contained->to_string()
        );
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    [[nodiscard]] usize alignment() const override {
        return 4;
    }

    [[nodiscard]] usize size_when_pushed() const override {
        return WordSize;
    }

    [[nodiscard]] bool is_pointer_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const PointerType*> as_pointer_type() const override {
        return this;
    }

    [[nodiscard]] bool contains_placeholders() const override {
        return contained->contains_placeholders();
    }

    DataType* contained;
    Mutability binding_mutability;
};

struct FunctionPointerType final : public DataType {
    explicit FunctionPointerType(std::vector<DataType*> parameter_types, DataType* return_type)
        : parameter_types{ std::move(parameter_types) },
          return_type{ return_type } { }

    [[nodiscard]] bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const FunctionPointerType*>(&other)) {
            if (parameter_types.size() != other_pointer->parameter_types.size()) {
                return false;
            }
            for (usize i = 0; i < parameter_types.size(); ++i) {
                if (*parameter_types[i] != *other_pointer->parameter_types[i]) {
                    return false;
                }
            }
            return *return_type == *other_pointer->return_type;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        using std::ranges::views::transform;
        return fmt::format(
                "Function({}) ~> {}",
                fmt::join(parameter_types | transform([](const auto& type) { return type->to_string(); }), ", "),
                return_type->to_string()
        );
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    [[nodiscard]] usize alignment() const override {
        return 4;
    }

    [[nodiscard]] usize size_when_pushed() const override {
        return WordSize;
    }

    [[nodiscard]] bool is_function_pointer_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const FunctionPointerType*> as_function_pointer_type() const override {
        return this;
    }

    [[nodiscard]] bool contains_placeholders() const override {
        for (const auto& type : parameter_types) {
            if (type->contains_placeholders()) {
                return true;
            }
        }
        return return_type->contains_placeholders();
    }

    std::vector<DataType*> parameter_types;
    DataType* return_type;
};
