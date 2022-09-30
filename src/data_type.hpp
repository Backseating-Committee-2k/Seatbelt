//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "mutability.hpp"
#include "type_container.hpp"
#include "types.hpp"
#include "utils.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <magic_enum.hpp>
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
struct PointerType;
struct FunctionPointerType;

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

    [[nodiscard]] virtual std::optional<const PrimitiveType*> as_primitive_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const ArrayType*> as_array_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const StructType*> as_struct_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const CustomType*> as_custom_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const PointerType*> as_pointer_type() const {
        return {};
    }

    [[nodiscard]] virtual std::optional<const FunctionPointerType*> as_function_pointer_Type() const {
        return {};
    }
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

    BasicType type;
};

struct ArrayType final : public DataType {
    ArrayType(const DataType* contained, usize num_elements) : contained{ contained }, num_elements{ num_elements } { }

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

    const DataType* contained;
    usize num_elements;
};

struct StructMember {
    std::string name;
    const DataType* data_type;

    [[nodiscard]] bool operator==(const StructMember& other) const {
        return data_type == other.data_type and name == other.name;
    }
};

struct StructType final : public DataType {
    StructType(
            std::string name,
            std::string namespace_qualifier,
            std::string custom_type_name,
            std::vector<StructMember> members
    )
        : name{ std::move(name) },
          namespace_qualifier{ std::move(namespace_qualifier) },
          custom_type_name{ std::move(custom_type_name) },
          members{ std::move(members) } { }

    [[nodiscard]] bool is_struct_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const StructType*> as_struct_type() const override {
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

    [[nodiscard]] usize size() const override {
        usize result = 0;
        for (const auto& member : members) {
            result = Utils::round_up(result, member.data_type->alignment());
            result += member.data_type->size();
        }
        return result;
    }

    [[nodiscard]] usize alignment() const override {
        if (members.empty()) {
            return 1;
        }
        const auto max_alignment_iterator =
                std::max_element(members.cbegin(), members.cend(), [](const auto& lhs, const auto& rhs) {
                    return lhs.data_type->alignment() < rhs.data_type->alignment();
                });
        return max_alignment_iterator->data_type->alignment();
    }

    [[nodiscard]] usize size_when_pushed() const override {
        const auto result =
                std::accumulate(members.cbegin(), members.cend(), usize{ 0 }, [](const usize sum, const auto& member) {
                    return sum + member.data_type->size_when_pushed();
                });
        assert(result % WordSize == 0);
        return result;
    }

    std::string name;
    std::string namespace_qualifier;
    std::string custom_type_name; // name of the custom type this struct was defined in
    std::vector<StructMember> members;
};

struct CustomType final : public DataType {
    CustomType(std::string name, std::string namespace_qualifier, std::vector<const StructType*> struct_types)
        : name{ std::move(name) },
          namespace_qualifier{ std::move(namespace_qualifier) },
          struct_types{ std::move(struct_types) } { }

    [[nodiscard]] bool is_custom_type() const override {
        return true;
    }

    [[nodiscard]] std::optional<const CustomType*> as_custom_type() const override {
        return this;
    }

    [[nodiscard]] bool is_tagged() const {
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
        for (usize i = 0; i < struct_types.size(); ++i) {
            if (not struct_types[i]->operator==(*(other_pointer->struct_types[i]))) {
                return false;
            }
        }
        return true;
    }

    [[nodiscard]] std::string to_string() const override {
        return namespace_qualifier + name;
    }

    [[nodiscard]] usize size() const override {
        assert(not struct_types.empty());
        const auto max_alignment_iterator =
                std::max_element(struct_types.cbegin(), struct_types.cend(), [](const auto& lhs, const auto& rhs) {
                    return lhs->size() < rhs->size();
                });
        return (*max_alignment_iterator)->size() + (is_tagged() ? WordSize : 0);
    }

    [[nodiscard]] usize alignment() const override {
        assert(not struct_types.empty());
        // this loop should get optimized away in release builds
        for (const auto& type : struct_types) {
            assert(type->alignment() <= WordSize);
        }

        if (is_tagged()) {
            return WordSize;
        }
        const auto max_alignment_iterator =
                std::max_element(struct_types.cbegin(), struct_types.cend(), [](const auto& lhs, const auto& rhs) {
                    return lhs->alignment() < rhs->alignment();
                });
        return (*max_alignment_iterator)->alignment();
    }

    [[nodiscard]] usize size_when_pushed() const override {
        assert(not struct_types.empty());
        const auto max_alignment_iterator =
                std::max_element(struct_types.cbegin(), struct_types.cend(), [](const auto& lhs, const auto& rhs) {
                    return lhs->size_when_pushed() < rhs->size_when_pushed();
                });
        return (*max_alignment_iterator)->size_when_pushed();
    }

    std::string name;
    std::string namespace_qualifier;
    std::vector<const StructType*> struct_types;
};

struct PointerType final : public DataType {
    PointerType(const DataType* contained, Mutability binding_mutability)
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

    const DataType* contained;
    Mutability binding_mutability;
};

struct FunctionPointerType final : public DataType {
    explicit FunctionPointerType(std::vector<const DataType*> parameter_types, const DataType* return_type)
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

    [[nodiscard]] std::optional<const FunctionPointerType*> as_function_pointer_Type() const override {
        return this;
    }

    std::vector<const DataType*> parameter_types;
    const DataType* return_type;
};
