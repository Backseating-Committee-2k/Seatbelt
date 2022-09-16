//
// Created by coder2k on 24.06.2022.
//

#pragma once

#include "bssembly.hpp"
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
        explicit Emitter(const Parser::Program* program, LabelGenerator* label_generator, TypeContainer* type_container)
            : program{ program },
              label_generator{ label_generator },
              type_container{ type_container } { }

        Bssembler::Bssembly operator()(const std::unique_ptr<Parser::ImportStatement>&) {
            return {};
        }

        Bssembler::Bssembly operator()(const std::unique_ptr<Parser::FunctionDefinition>& function_definition);

        Bssembler::Bssembly operator()(const std::unique_ptr<Parser::CustomTypeDefinition>&) {
            return {};
        }

        Bssembler::Bssembly operator()(const std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition);

        const Parser::Program* program;
        LabelGenerator* label_generator;
        TypeContainer* type_container;
    };

} // namespace Emitter
