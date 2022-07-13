//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <string_view>

struct DataType {
protected:
    explicit DataType(bool is_mutable) : is_mutable{ is_mutable } { }

public:
    virtual ~DataType() = default;

    virtual bool operator==(const DataType& other) const {
        return is_mutable == other.is_mutable;
    }

    [[nodiscard]] virtual std::string to_string() const {
        return is_mutable ? "mutable " : "const ";
    }

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

    std::unique_ptr<DataType> contained;
};
