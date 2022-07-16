//
// Created by coder2k on 12.07.2022.
//

#include "scope_generator.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "type_checker.hpp"
#include "types.hpp"
#include <algorithm>
#include <fmt/core.h>
#include <ranges>

namespace ScopeGenerator {

    struct ScopeGenerator : public Parser::Statements::StatementVisitor, public Parser::Expressions::ExpressionVisitor {
        ScopeGenerator(Scope* scope, usize offset, TypeContainer* type_container)
            : scope{ scope },
              offset{ offset },
              type_container{ type_container } { }

        void visit(Parser::Statements::Block& statement) override {
            for (auto& sub_statement : statement.statements) {
                if (const auto sub_block = dynamic_cast<Parser::Statements::Block*>(sub_statement.get())) {
                    sub_block->scope = std::make_unique<Scope>(scope);
                    auto sub_visitor = ScopeGenerator{ sub_block->scope.get(), offset, type_container };
                    sub_block->accept(sub_visitor);
                    offset = sub_visitor.offset;
                } else {
                    sub_statement->accept(*this);
                }
            }
        }

        void visit(Parser::Statements::VariableDefinition& statement) override {
            if (scope->contains(statement.name.location.view())) {
                Error::error(
                        statement.name, fmt::format("redefinition of identifier \"{}\"", statement.name.location.view())
                );
            }
            statement.initial_value->accept(*this);
            const auto data_type = type_container->from_tokens(statement.type_tokens);
            (*scope)[statement.name.location.view()] = VariableSymbol{ .offset{ offset }, .data_type{ data_type } };
            offset += 4;// TODO: different data types
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::InlineAssembly& statement) override {
            statement.surrounding_scope = scope;
        }

        void visit(Parser::Statements::ExpressionStatement& statement) override {
            statement.surrounding_scope = scope;
            statement.expression->accept(*this);
        }

        void visit(Parser::Expressions::Literal& expression) override {
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::Name& expression) override {
            expression.surrounding_scope = scope;
            const Scope* current_scope = expression.surrounding_scope;
            const auto identifier = expression.name.location.view();
            while (current_scope != nullptr) {
                const auto find_iterator = std::find_if(
                        std::cbegin(*current_scope), std::cend(*current_scope),
                        [identifier](const auto& pair) { return pair.first == identifier; }
                );
                const auto identifier_found = find_iterator != std::cend(*current_scope);
                if (identifier_found) {
                    struct {
                        const DataType* operator()(const VariableSymbol& variable) {
                            return variable.data_type;
                        }

                        const DataType* operator()(const FunctionSymbol& function) {
                            // we cannot determine the return type of the function here because
                            // we cannot do overload resolution as of now
                            return nullptr;
                        }
                    } symbol_visitor;

                    expression.definition_data_type = std::visit(symbol_visitor, find_iterator->second);
                    return;
                }
                current_scope = current_scope->surrounding_scope;
            }
            // at this point it is still possible that the identifier refers to a function
            // which we haven't seen yet, so this must not be an error
        }

        void visit(Parser::Expressions::BinaryOperator& expression) override {
            expression.lhs->accept(*this);
            expression.rhs->accept(*this);
            expression.surrounding_scope = scope;
        }

        void visit(Parser::Expressions::FunctionCall& expression) override {
            expression.callee->accept(*this);
            for (auto& argument : expression.arguments) {
                argument->accept(*this);
            }
            expression.surrounding_scope = scope;
        }

        Scope* scope;
        usize offset;
        TypeContainer* type_container;
    };

    struct TopLevelScopeGeneratorVisitor {
        TopLevelScopeGeneratorVisitor(Parser::Program* program, TypeContainer* type_container, Scope* global_scope)
            : program{ program },
              type_container{ type_container },
              global_scope{ global_scope } { }

        void operator()(std::unique_ptr<Parser::FunctionDefinition>& function_definition) {
            using namespace std::string_literals;
            using std::ranges::find_if;

            auto function_scope = std::make_unique<Scope>(global_scope);
            usize offset = 0;
            for (auto& parameter : function_definition->parameters) {
                if (function_scope->contains(parameter.name.location.view())) {
                    Error::error(
                            parameter.name,
                            fmt::format("duplicate parameter name \"{}\"", parameter.name.location.view())
                    );
                }
                const auto parameter_type = type_container->from_tokens(parameter.type_tokens);
                (*function_scope)[parameter.name.location.view()] =
                        VariableSymbol{ .offset{ offset }, .data_type{ parameter_type } };
                offset += 4;// TODO: different data types
            }
            auto identifier = function_definition->name.location.view();
            auto find_iterator = find_if(*global_scope, [&](const auto& pair) { return pair.first == identifier; });
            const auto found = find_iterator != std::end(*global_scope);
            auto function_overload = FunctionOverload{};
            if (found) {
                assert(std::holds_alternative<FunctionSymbol>(find_iterator->second) &&
                       "other cases not implemented yet");
                auto& function_symbol = std::get<FunctionSymbol>(find_iterator->second);
                auto& new_overload = function_symbol.overloads.emplace_back(std::move(function_overload));
                function_definition->corresponding_symbol = &new_overload;
            } else {
                auto new_symbol = FunctionSymbol{ .overloads{ { std::move(function_overload) } } };
                function_definition->corresponding_symbol = &new_symbol.overloads.back();
                (*global_scope)[identifier] = std::move(new_symbol);
            }

            auto visitor = ScopeGenerator{ function_scope.get(), offset, type_container };
            function_definition->body.scope = std::move(function_scope);
            function_definition->body.accept(visitor);
        }

        void operator()(std::unique_ptr<Parser::ImportStatement>& function_definition) { }

        Parser::Program* program;
        TypeContainer* type_container;
        Scope* global_scope;
    };

    void generate(Parser::Program& program, TypeContainer& type_container, Scope& global_scope) {
        auto visitor = TopLevelScopeGeneratorVisitor{ &program, &type_container, &global_scope };
        for (auto& top_level_statement : program) {
            std::visit(visitor, top_level_statement);
        }
    }

}// namespace ScopeGenerator
