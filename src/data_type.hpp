//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "type_container.hpp"
#include "types.hpp"
#include <cassert>
#include <fmt/core.h>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

static constexpr std::string_view U32Identifier{ "U32" };
static constexpr std::string_view CharIdentifier{ "Char" };
static constexpr std::string_view BoolIdentifier{ "Bool" };
static constexpr std::string_view NothingIdentifier{ "Nothing" };

enum class Mutability {
    Mutable,
    Const,
};

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
            return DataType::operator==(other) and contained == other_pointer->contained;
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

struct FunctionPointerType final : public DataType {
    explicit FunctionPointerType(std::string signature, Mutability mutability)
        : DataType{ mutability },
          signature{ std::move(signature) } { }

    bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const FunctionPointerType*>(&other)) {
            return DataType::operator==(other) and signature == other_pointer->signature;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return fmt::format("{}function pointer with signature \"{}\"", DataType::to_string(), signature);
    }

    [[nodiscard]] std::string mangled_name() const override {
        return fmt::format("$function_pointer${}", signature);
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    const DataType* as_mutable(TypeContainer& type_container) const override {
        if (is_mutable()) {
            return this;
        }
        return type_container.from_type_definition(std::make_unique<FunctionPointerType>(signature, Mutability::Mutable)
        );
    }

    const DataType* as_const(TypeContainer& type_container) const override {
        if (is_const()) {
            return this;
        }
        return type_container.from_type_definition(std::make_unique<FunctionPointerType>(signature, Mutability::Const));
    }

    std::string signature;
};
