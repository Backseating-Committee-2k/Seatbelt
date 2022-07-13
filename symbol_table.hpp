//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "types.hpp"
#include "data_type.hpp"
#include <unordered_map>
#include <string_view>

struct SymbolDescription {
    usize offset{ 0 };
    const DataType* data_type{ nullptr };
};

struct SymbolTable : public std::unordered_map<std::string_view, SymbolDescription> {
    explicit SymbolTable(const SymbolTable* surrounding_scope)
        : std::unordered_map<std::string_view, SymbolDescription>::unordered_map{},
          surrounding_scope{ surrounding_scope } { }

    const SymbolTable* surrounding_scope;
};
