//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "types.hpp"
#include <unordered_map>
#include <string_view>

/*
struct SymbolDescription {
    usize offset;
    std::unique_ptr<DataType> data_type;
};

TODO: next time
TODO: also: RUN THE SCOPE GENERATOR!

*/

struct SymbolTable : public std::unordered_map<std::string_view, usize> {
    explicit SymbolTable(const SymbolTable* surrounding_scope)
        : std::unordered_map<std::string_view, usize>::unordered_map{},
          surrounding_scope{ surrounding_scope } { }

    const SymbolTable* surrounding_scope;
};
