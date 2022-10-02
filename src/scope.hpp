//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "data_type.hpp"
#include "parameter_list.hpp"
#include "types.hpp"
#include <list>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <variant>

namespace Parser {
    struct FunctionDefinition;
    struct NamespaceDefinition;
    struct CustomTypeDefinition;

    namespace Statements {
        struct VariableDefinition;
    }
} // namespace Parser

struct VariableSymbol {
    std::optional<usize> offset{};
    std::variant<std::monostate, const Parser::Statements::VariableDefinition*, const Parameter*> definition{};
};

struct CustomTypeSymbol {
    Parser::CustomTypeDefinition* definition{ nullptr };
};

struct StructSymbol {
    Parser::CustomTypeDefinition* custom_type_definition;
    u32 tag;
};

struct FunctionOverload {
    std::string signature{};
    const Parser::NamespaceDefinition* surrounding_namespace{ nullptr };
    Parser::FunctionDefinition* definition{ nullptr };
};

struct FunctionSymbol {
    std::list<FunctionOverload> overloads;
};

struct NamespaceSymbol {
    const Parser::NamespaceDefinition* namespace_definition{ nullptr };
};

using SymbolDescription = std::variant<VariableSymbol, CustomTypeSymbol, StructSymbol, FunctionSymbol, NamespaceSymbol>;

struct Scope : public std::unordered_map<std::string_view, SymbolDescription> {
    explicit Scope(const Scope* surrounding_scope, const Parser::NamespaceDefinition* surrounding_namespace)
        : std::unordered_map<std::string_view, SymbolDescription>::unordered_map{},
          surrounding_scope{ surrounding_scope },
          surrounding_namespace{ surrounding_namespace } { }

    [[nodiscard]] std::unique_ptr<Scope> create_child_scope() {
        return std::make_unique<Scope>(this, surrounding_namespace);
    }

    [[nodiscard]] bool is_child_of(const Scope* const parent) const {
        const Scope* current = this;
        while (current != nullptr) {
            if (current != this and current == parent) {
                return true;
            }
            current = current->surrounding_scope;
        }
        return false;
    }

    const Scope* surrounding_scope;
    const Parser::NamespaceDefinition* surrounding_namespace;
};

[[nodiscard]] const SymbolDescription* scope_lookup(const Scope* scope, std::string_view identifier);
