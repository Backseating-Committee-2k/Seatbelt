//
// Created by coder2k on 16.07.2022.
//

#include "namespace.hpp"
#include <fmt/core.h>
#include <fmt/format.h>

using Parser::Expressions::Name;

[[nodiscard]] std::string get_namespace_qualifier(const Name& expression) {
    auto qualified_name = std::string{ expression.surrounding_scope->surrounding_namespace };
    const auto inside_global_namespace = qualified_name.empty();
    return qualified_name + (not inside_global_namespace ? "%" : "") + get_absolute_namespace_qualifier(expression);
}

[[nodiscard]] std::string get_absolute_namespace_qualifier(const Name& expression) {
    using Lexer::Tokens::Identifier;
    auto qualified_name = std::string{};
    assert(not expression.name_tokens.empty());
    for (usize i = 0; i < expression.name_tokens.size() - 1; i += 2) {
        const auto& name = std::get<Identifier>(expression.name_tokens[i]);
        if (i > 0) {
            qualified_name += "%";
        }
        qualified_name += name.location.view();
    }
    return qualified_name;
}

[[nodiscard]] std::string get_namespace_qualifier(const NamespacesStack& namespaces_stack) {
    return fmt::format("{}", fmt::join(namespaces_stack, "%"));
}
