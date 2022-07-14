//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "data_type.hpp"
#include "parameter_list.hpp"
#include "parser.hpp"
#include "types.hpp"
#include <list>
#include <string_view>
#include <unordered_map>
#include <variant>

struct VariableSymbol {
    usize offset{ 0 };
    const DataType* data_type{ nullptr };
};

struct FunctionOverload {
    std::string signature{};
    const ParameterList* parameters{ nullptr };
    const DataType* return_type{ nullptr };
};

struct FunctionSymbol {
    std::list<FunctionOverload> overloads;
};

using SymbolDescription = std::variant<VariableSymbol, FunctionSymbol>;

struct Scope : public std::unordered_map<std::string_view, SymbolDescription> {
    explicit Scope(const Scope* surrounding_scope)
        : std::unordered_map<std::string_view, SymbolDescription>::unordered_map{},
          surrounding_scope{ surrounding_scope } { }

    const Scope* surrounding_scope;
};
