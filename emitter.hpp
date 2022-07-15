//
// Created by coder2k on 24.06.2022.
//

#pragma once

#include "parser.hpp"
#include <string>

namespace Emitter {

    struct Emitter {
        explicit Emitter(const Parser::Program* program) : program{ program } { }

        std::string operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition) const;

        std::string operator()(const std::unique_ptr<Parser::ImportStatement>& function_definition) const;

        const Parser::Program* program;
    };

}// namespace Emitter
