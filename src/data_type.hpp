//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "mutability.hpp"
#include "type_container.hpp"
#include "types.hpp"
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

static constexpr std::string_view U32Identifier{ "U32" };
static constexpr std::string_view CharIdentifier{ "Char" };
static constexpr std::string_view BoolIdentifier{ "Bool" };
static constexpr std::string_view NothingIdentifier{ "Nothing" };
static constexpr std::string_view FunctionPointerKeyword{ "Function" };

struct DataType {
public:
    virtual ~DataType() = default;

    virtual bool operator==(const DataType& other) const = 0;
    [[nodiscard]] virtual std::string to_string() const = 0;
    [[nodiscard]] virtual usize size() const = 0;
    [[nodiscard]] virtual usize alignment() const = 0;

    [[nodiscard]] virtual usize num_words() const {
        assert(size() % 4 == 0);
        return size() / 4;
    }

    [[nodiscard]] virtual bool is_concrete_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_pointer_type() const {
        return false;
    }

    [[nodiscard]] virtual bool is_function_pointer_type() const {
        return false;
    }
};

struct ConcreteType final : public DataType {
    explicit ConcreteType(std::string_view name) : name{ name } { }

    bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const ConcreteType*>(&other)) {
            return name == other_pointer->name;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return std::string{ name };
    }

    [[nodiscard]] usize size() const override {
        if (name == U32Identifier) {
            return 4;
        } else if (name == BoolIdentifier or name == CharIdentifier) {
            return 1;
        } else if (name == NothingIdentifier) {
            return 0;
        } else {
            assert(false and "unknown data type");
            return {};
        }
    }

    [[nodiscard]] usize alignment() const override {
        return std::max(size(), usize{ 1 });
    }

    [[nodiscard]] bool is_concrete_type() const override {
        return true;
    }

    std::string_view name;
};

struct PointerType final : public DataType {
    PointerType(const DataType* contained, Mutability binding_mutability)
        : contained{ contained },
          binding_mutability{ binding_mutability } { }

    bool operator==(const DataType& other) const override {
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

    [[nodiscard]] bool is_pointer_type() const override {
        return true;
    }

    const DataType* contained;
    Mutability binding_mutability;
};

struct FunctionPointerType final : public DataType {
    explicit FunctionPointerType(std::vector<const DataType*> parameter_types, const DataType* return_type)
        : parameter_types{ std::move(parameter_types) },
          return_type{ return_type } { }

    bool operator==(const DataType& other) const override {
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

    [[nodiscard]] bool is_function_pointer_type() const override {
        return true;
    }

    std::vector<const DataType*> parameter_types;
    const DataType* return_type;
};
