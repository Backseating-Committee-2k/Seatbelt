//
// Created by coder2k on 12.07.2022.
//

#pragma once

#include "parser.hpp"
#include "type_container.hpp"

namespace ScopeGenerator {

    void generate(Parser::Program& program, TypeContainer& type_container, Scope& global_scope);

} // namespace ScopeGenerator
