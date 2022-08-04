//
// Created by coder2k on 14.07.2022.
//

#include "scope.hpp"

[[nodiscard]] const SymbolDescription* scope_lookup(const Scope* scope, const std::string_view identifier) {
    while (scope != nullptr) {
        const auto find_iterator = scope->find(identifier);
        const auto found = find_iterator != std::cend(*scope);
        if (found) {
            return &find_iterator->second;
        }
        scope = scope->surrounding_scope;
    }
    return nullptr;
}
