//
// Created by coder2k on 16.07.2022.
//

#pragma once

#include "parser.hpp"
#include <string>

using NamespacesStack = std::vector<std::string>;

[[nodiscard]] std::string get_namespace_qualifier(const Parser::Expressions::Name& expression);
[[nodiscard]] std::string get_absolute_namespace_qualifier(const Parser::Expressions::Name& expression);
[[nodiscard]] std::string get_namespace_qualifier(const NamespacesStack& namespaces_stack);
