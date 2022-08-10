//
// Created by coder2k on 12.07.2022.
//

#pragma once

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

enum class Mutability {
    Mutable,
    Const,
};

[[nodiscard]] inline bool is_const(const Mutability mutability) {
    return mutability == Mutability::Const;
}

[[nodiscard]] inline bool is_mutable(const Mutability mutability) {
    return mutability == Mutability::Mutable;
}

struct DataType {
protected:
    explicit DataType(Mutability mutability) : mutability{ mutability } { }

public:
    virtual ~DataType() = default;

    virtual bool operator==(const DataType& other) const {
        return mutability == other.mutability;
    }

    [[nodiscard]] bool is_const() const {
        return mutability == Mutability::Const;
    }

    [[nodiscard]] bool is_mutable() const {
        return not is_const();
    }

    [[nodiscard]] virtual const DataType* as_mutable(TypeContainer& type_container) const = 0;

    [[nodiscard]] virtual const DataType* as_const(TypeContainer& type_container) const = 0;

    [[nodiscard]] virtual std::string to_string() const {
        switch (mutability) {
            case Mutability::Mutable:
                return "mutable";
            case Mutability::Const:
                return "const";
        }
        assert(false and "unreachable");// todo: replace with std::unreachable
        return "";
    }

    [[nodiscard]] virtual std::string mangled_name() const = 0;

    [[nodiscard]] virtual usize size() const = 0;

    Mutability mutability;
};

struct ConcreteType final : public DataType {
    ConcreteType(std::string_view name, Mutability mutability) : DataType{ mutability }, name{ name } { }

    bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const ConcreteType*>(&other)) {
            return DataType::operator==(other) and name == other_pointer->name;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return fmt::format("{} {}", DataType::to_string(), std::string{ name });
    }

    [[nodiscard]] std::string mangled_name() const override {
        return std::string{ name };
    }

    [[nodiscard]] usize size() const override {
        if (name == U32Identifier or name == BoolIdentifier or name == CharIdentifier) {
            return 4;
        } else if (name == NothingIdentifier) {
            return 0;
        } else {
            return {};
        }
    }

    const DataType* as_mutable(TypeContainer& type_container) const override {
        if (is_mutable()) {
            return this;
        }
        return type_container.from_type_definition(std::make_unique<ConcreteType>(name, Mutability::Mutable));
    }

    const DataType* as_const(TypeContainer& type_container) const override {
        if (is_const()) {
            return this;
        }
        return type_container.from_type_definition(std::make_unique<ConcreteType>(name, Mutability::Const));
    }

    std::string_view name;
};

struct PointerType final : public DataType {
    PointerType(const DataType* contained, Mutability mutability) : DataType{ mutability }, contained{ contained } { }

    bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const PointerType*>(&other)) {
            return DataType::operator==(other) and *contained == *other_pointer->contained;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        using namespace std::string_literals;
        assert(contained);
        return DataType::to_string() + "->"s + contained->to_string();
    }

    [[nodiscard]] std::string mangled_name() const override {
        assert(contained);
        return fmt::format("->{}", contained->mangled_name());
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    const DataType* as_mutable(TypeContainer& type_container) const override {
        if (is_mutable()) {
            return this;
        }
        return type_container.from_type_definition(std::make_unique<PointerType>(contained, Mutability::Mutable));
    }

    const DataType* as_const(TypeContainer& type_container) const override {
        if (is_const()) {
            return this;
        }
        return type_container.from_type_definition(std::make_unique<PointerType>(contained, Mutability::Const));
    }

    const DataType* contained;
};

struct FunctionPointerType : public DataType {
    explicit FunctionPointerType(
            std::vector<const DataType*> parameter_types,
            const DataType* return_type,
            Mutability mutability
    )
        : DataType{ mutability },
          parameter_types{ parameter_types },
          return_type{ return_type } { }

    bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const FunctionPointerType*>(&other)) {
            if (not DataType::operator==(other)) {
                return false;
            }
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
                "{} Function({}) ~> {}", DataType::to_string(),
                fmt::join(parameter_types | transform([](const auto& type) { return type->to_string(); }), ", "),
                return_type->to_string()
        );
    }

    [[nodiscard]] std::string mangled_name() const override {
        using std::ranges::views::transform;
        return fmt::format(
                "Function({}) ~> {}",
                fmt::join(parameter_types | transform([](const auto& type) { return type->mangled_name(); }), ", "),
                return_type->mangled_name()
        );
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    const DataType* as_mutable(TypeContainer& type_container) const override {
        if (is_mutable()) {
            return this;
        }
        return type_container.from_type_definition(
                std::make_unique<FunctionPointerType>(parameter_types, return_type, Mutability::Mutable)
        );
    }

    const DataType* as_const(TypeContainer& type_container) const override {
        if (is_const()) {
            return this;
        }
        return type_container.from_type_definition(
                std::make_unique<FunctionPointerType>(parameter_types, return_type, Mutability::Const)
        );
    }

    std::vector<const DataType*> parameter_types;
    const DataType* return_type;
};
