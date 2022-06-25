//
// Created by coder2k on 24.06.2022.
//

#pragma once

#include "parser.hpp"
#include <string>

namespace Emitter {

    struct Emitter {
        std::string operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) const;
    };

}// namespace Emitter
