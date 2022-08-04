//
// Created by coder2k on 12.07.2022.
//

#pragma once

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

struct DataType {
protected:
    explicit DataType(bool is_mutable) : is_mutable{ is_mutable } { }

public:
    virtual ~DataType() = default;

    virtual bool operator==(const DataType& other) const {
        return is_mutable == other.is_mutable;
    }

    [[nodiscard]] std::unique_ptr<DataType> as_mutable() const {
        auto copy = clone();
        copy->is_mutable = true;
        return copy;
    }

    [[nodiscard]] std::unique_ptr<DataType> as_const() const {
        auto copy = clone();
        copy->is_mutable = false;
        return copy;
    }

    [[nodiscard]] virtual std::unique_ptr<DataType> clone() const = 0;

    [[nodiscard]] virtual std::string to_string() const {
        return is_mutable ? "mutable " : "const ";
    }

    [[nodiscard]] virtual std::string mangled_name() const = 0;

    [[nodiscard]] virtual usize size() const = 0;

    bool is_mutable;
};

struct ConcreteType : public DataType {
    ConcreteType(std::string_view name, bool is_mutable) : DataType{ is_mutable }, name{ std::move(name) } { }

    bool operator==(const DataType& other) const override {
        if (const auto other_pointer = dynamic_cast<const ConcreteType*>(&other)) {
            return DataType::operator==(other) and name == other_pointer->name;
        }
        return false;
    }

    [[nodiscard]] std::string to_string() const override {
        return DataType::to_string() + std::string{ name };
    }

    [[nodiscard]] std::string mangled_name() const override {
        return fmt::format("${}", name);
    }

    [[nodiscard]] std::unique_ptr<DataType> clone() const final {
        return std::make_unique<ConcreteType>(name, is_mutable);
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

    std::string_view name;
};

struct PointerType : public DataType {
    PointerType(std::unique_ptr<DataType> contained, bool is_mutable)
        : DataType{ is_mutable },
          contained{ std::move(contained) } { }

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
        return fmt::format("$pointer${}", contained->mangled_name());
    }

    [[nodiscard]] std::unique_ptr<DataType> clone() const final {
        return std::make_unique<PointerType>(contained->clone(), is_mutable);
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    std::unique_ptr<DataType> contained;
};

struct FunctionPointerType : public DataType {
    explicit FunctionPointerType(std::string signature, bool is_mutable)
        : DataType{ is_mutable },
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

    [[nodiscard]] std::unique_ptr<DataType> clone() const final {
        return std::make_unique<FunctionPointerType>(signature, is_mutable);
    }

    [[nodiscard]] usize size() const override {
        return 4;
    }

    std::string signature;
};
