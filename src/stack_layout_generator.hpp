//
// Created by coder2k on 11.09.2022.
//

#pragma once

#include "parser.hpp"
#include "utils.hpp"

namespace StackLayoutGenerator {

    struct StackLayoutGeneratorVisitor : Parser::Statements::StatementVisitor {
        explicit StackLayoutGeneratorVisitor(usize starting_offset) {
            if (starting_offset > 0) {
                // alignment is 1, because we assume that the starting offset already has the right alignment
                claim_stack_space(starting_offset, 1);
            }
        }

        void visit(Parser::Statements::Block& statement) override {
            const auto old_offset = offset;
            for (auto& sub_statement : statement.statements) {
                sub_statement->accept(*this);
            }
            statement.occupied_stack_space = occupied_stack_space;
            offset = old_offset;
        }

        void visit(Parser::Statements::IfStatement& statement) override {
            statement.then_block.accept(*this);
            statement.else_block.accept(*this);
        }

        void visit(Parser::Statements::LoopStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::BreakStatement&) override { }

        void visit(Parser::Statements::ContinueStatement&) override { }

        void visit(Parser::Statements::WhileStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::DoWhileStatement& statement) override {
            statement.body.accept(*this);
        }

        void visit(Parser::Statements::ForStatement& statement) override {
            const auto old_offset = offset;
            if (statement.initializer) {
                statement.initializer->accept(*this);
            }
            statement.body.accept(*this);
            offset = old_offset;
        }

        void visit(Parser::Statements::ReturnStatement&) override { }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            assert(statement.variable_symbol != nullptr);
            statement.variable_symbol->offset =
                    claim_stack_space(statement.data_type->size(), statement.data_type->alignment());
        }

        void visit(Parser::Statements::InlineAssembly&) override { }

        void visit(Parser::Statements::ExpressionStatement&) override { }

        void visit(Parser::Statements::LabelDefinition&) override { }

        void visit(Parser::Statements::GotoStatement&) override { }

        usize claim_stack_space(const usize size_of_type, const usize alignment) {
            // we have to make sure that the offset has the right alignment
            const auto old_offset = Utils::round_up(offset, alignment);
            offset = old_offset + size_of_type;
            if (offset > occupied_stack_space) {
                occupied_stack_space = offset;
            }
            return old_offset;
        }

        usize offset{ 0 };
        usize occupied_stack_space{ 0 };
    };

    inline void generate_stack_layout(Parser::Program& program) {
        for (auto& top_level_statement : program) {
            std::visit(
                    overloaded{ [](const std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
                                   usize size_of_parameters =
                                           (function_definition->return_type->size_when_pushed() > WordSize ? WordSize
                                                                                                            : 0);
                                   for (const auto& parameter : function_definition->parameters) {
                                       size_of_parameters =
                                               Utils::round_up(size_of_parameters, parameter.data_type->alignment());
                                       size_of_parameters += parameter.data_type->size();
                                   }
                                   auto visitor = StackLayoutGeneratorVisitor{ size_of_parameters };
                                   function_definition->body.accept(visitor);
                                   function_definition->occupied_stack_space =
                                           Utils::round_up(visitor.occupied_stack_space, WordSize);
                                   function_definition->parameters_stack_space = size_of_parameters;
                               },
                                [](const std::unique_ptr<Parser::NamespaceDefinition>& namespace_definition) {
                                    generate_stack_layout(namespace_definition->contents);
                                },
                                [](const auto&) {} },
                    top_level_statement
            );
        }
    }

} // namespace StackLayoutGenerator
