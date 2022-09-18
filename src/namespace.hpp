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

[[nodiscard]] std::string get_namespace_qualifier(const NamespacesStack& namespaces_stack);

/**
 * Walks the namespace hierarchy from inside to outside starting from the given namespace and returns a string
 * of the form ::outer::inner:: (including the trailing double colon).
 * @param namespace_definition The starting namespace.
 * @return The absolute namespace path as string.
 */
[[nodiscard]] std::string get_absolute_namespace_qualifier(const Parser::NamespaceDefinition& namespace_definition);
