//
// Created by coder2k on 16.07.2022.
//

#pragma once

#include "parser.hpp"
#include <string>

using NamespacesStack = std::vector<std::string>;

/**
 * Gets the namespace qualifier of the passed name expression. For example a name consisting of a::b::test that is
 * surrounded by a namespace ::n::m:: will result in a string "::n::m::a::b::" (note the missing "test", that is
 * not part of the namespace).
 * @param expression The name expression.
 * @return The qualifier of the name, starting from the global namespace, as string.
 */
[[nodiscard]] std::string get_namespace_qualifier(const Parser::Expressions::Name& expression);

/**
 * Gets the namespace qualifier of the passed name expression as if it was surrounded. For example a name consisting
 * of a::b::test that is surrounded by a namespace ::n::m:: will result in a string "::a::b::" (note the missing
 * "test" and the missing "n::m::"). This is a helper function used to do a "fallback-lookup": If a name is not
 * reachable from its current location, it is possible to start the search again, starting from the global namespace.
 * @param expression The name expression.
 * @return The qualifier of the name as if it was surrounded by the global namespace, as string.
 */
[[nodiscard]] std::string get_absolute_namespace_qualifier(const Parser::Expressions::Name& expression);

[[nodiscard]] std::string get_namespace_qualifier(const NamespacesStack& namespaces_stack);
