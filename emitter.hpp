//
// Created by coder2k on 24.06.2022.
//

#pragma once

#include "parser.hpp"
#include <string>

namespace Emitter {

    struct LabelGenerator {
        [[nodiscard]] std::string next_label(const std::string_view tag) {
            return fmt::format("${}${}", label_counter++, tag);
        }

    private:
        usize label_counter{ 0 };
    };

    struct Emitter {
        explicit Emitter(const Parser::Program* program, LabelGenerator* label_generator)
            : program{ program },
              label_generator{ label_generator } { }

        std::string operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition);

        std::string operator()(const std::unique_ptr<Parser::ImportStatement>& function_definition);

        const Parser::Program* program;
        LabelGenerator* label_generator;
    };

}// namespace Emitter
