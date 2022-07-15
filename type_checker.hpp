//
// Created by coder2k on 10.07.2022.
//

#pragma once

#include "parser.hpp"
#include "type_container.hpp"
#include <memory>

namespace TypeChecker {
    void check(Parser::Program& program, TypeContainer& type_container, const Scope& global_scope);
}// namespace TypeChecker
