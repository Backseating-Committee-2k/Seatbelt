//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "namespace.hpp"
#include "parser.hpp"
#include "type_container.hpp"
#include <algorithm>
#include <ranges>

namespace ScopeGenerator {

    template<typename Type, typename Symbol>
    [[nodiscard]] std::vector<const Type*> namespace_based_lookup(
            std::string_view namespace_qualifier,
            const Parser::Expressions::Name& name,
            const Symbol& symbol
    ) {
        using std::ranges::count;
        // We *did* find a function symbol with the correct function name (even though we
        // do not know the function signature), but the function can only be a valid choice
        // if it is within the correct namespace

        auto remaining_namespaces = namespace_qualifier.empty() ? 0 : count(namespace_qualifier, ':') / 2;
        const auto was_qualified_name = (name.name_tokens.size() > 1);
        auto possible_overloads = std::vector<const Type*>{};
        while (true) {
            // Function definitions are only allowed in the global scope. That means
            // that we must be at the top of the scope stack right now.
            for (const auto& overload : symbol.overloads) {
                if (overload.namespace_name == namespace_qualifier) {
                    // we found a possible overload!

                    // we cannot determine the return type of the function here because
                    // we cannot do type-based overload resolution as of now

                    // if the namespace the name expression occurred in is different from the namespace
                    // in which we found the function overload, then the lookup is only valid if the function
                    // has been exported
                    if (name.surrounding_scope->surrounding_namespace.starts_with(namespace_qualifier)
                        or overload.definition->is_exported()) {
                        possible_overloads.push_back(&overload);
                    }
                }
            }

            // if the name is a qualified name (i.e. with specified namespace) it is
            // not valid to make a lookup in the outer namespaces (except for the
            // global namespace, but that case is handled separately later on)
            if (was_qualified_name) {
                break;
            }

            if (remaining_namespaces == 1) {
                break;
            }
            const auto max_pos = namespace_qualifier.length() >= 3 ? namespace_qualifier.length() - 3
                                                                   : decltype(namespace_qualifier)::npos;
            const auto end_index = namespace_qualifier.rfind(':', max_pos);
            assert(end_index != decltype(namespace_qualifier)::npos);
            namespace_qualifier = namespace_qualifier.substr(0, end_index + 1);
            --remaining_namespaces;
        }

        // if we couldn't find any overloads yet and this is a qualified name,
        // then we can also look into the global namespace and try to find an
        // exact match in there
        const auto is_global_lookup_fallback = possible_overloads.empty();

        const auto inside_global_namespace = name.surrounding_scope->surrounding_namespace == "::";

        if (was_qualified_name) {
            const auto global_name_qualifier = get_absolute_namespace_qualifier(name);
            for (const auto& overload : symbol.overloads) {
                if (overload.namespace_name == global_name_qualifier) {
                    // we found a possible overload!

                    // we cannot determine the return type of the function here because
                    // we cannot do type-based overload resolution as of now
                    if (is_global_lookup_fallback) {
                        // during fall-back, only exported functions are valid
                        if (overload.definition->is_exported()) {
                            possible_overloads.push_back(&overload);
                        }
                    } else if (not inside_global_namespace) {
                        Error::warning(
                                name.name_tokens.back(),
                                "absolute/relative namespace ambiguity can lead to "
                                "confusion"
                        );
                    }
                }
            }
        }

        if (possible_overloads.empty()) {
            Error::error(name.name_tokens.back(), "no matching function overload found");
        }
        return possible_overloads;
    }

    void generate(Parser::Program& program, TypeContainer& type_container, Scope& global_scope);

} // namespace ScopeGenerator
