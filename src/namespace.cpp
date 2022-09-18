//
// Created by coder2k on 16.07.2022.
//

#include "namespace.hpp"
#include "error.hpp"
#include <fmt/core.h>
#include <fmt/format.h>
#include <ranges>

using Parser::Expressions::Name;

[[nodiscard]] std::string get_namespace_qualifier(const Name& expression) {
    using std::ranges::views::transform, std::ranges::views::take;
    return fmt::format(
            "{}{}", get_absolute_namespace_qualifier(*(expression.surrounding_scope->surrounding_namespace)),
            fmt::join(
                    expression.name_tokens | take(expression.name_tokens.size() - 1)
                            | transform([](auto token) { return Error::token_location(token).view(); }),
                    ""
            )
    );
}

[[nodiscard]] std::string get_namespace_qualifier(const NamespacesStack& namespaces_stack) {
    auto result = std::string{ "::" };
    for (const auto& space : namespaces_stack) {
        result += space + "::";
    }
    return result;
}

[[nodiscard]] std::string get_absolute_namespace_qualifier(const Parser::NamespaceDefinition& namespace_definition) {
    auto result = std::string{};
    auto current_namespace = &namespace_definition;
    while (true) {
        result = fmt::format("{}::{}", current_namespace->name.location.view(), result);
        if (current_namespace->scope->surrounding_scope == nullptr) {
            break;
        }
        current_namespace = current_namespace->scope->surrounding_scope->surrounding_namespace;
    }

    return result;
}
