//
// Created by coder2k on 17.06.2022.
//

#pragma once

#include <vector>
#include <variant>
#include <span>
#include <memory>
#include "lexer.hpp"

namespace Parser {

    using namespace Lexer::Token;

    using Type = std::span<const Token>;

    struct Statement {
        virtual ~Statement() = default;
    };

    using StatementList = std::vector<std::unique_ptr<Statement>>;

    struct Block : public Statement {
        explicit Block(StatementList statements) : statements{ std::move(statements) } { }

        StatementList statements;
    };

    struct VariableDefinition : public Statement {
        VariableDefinition(Identifier name, Type type, IntegerLiteral initial_value)
            : name{ name },
              type{ type },
              initial_value{ initial_value } { }

        Identifier name;
        Type type;
        IntegerLiteral initial_value;
    };

    struct Parameter {
        Identifier name;
        Type type;
    };

    using ParameterList = std::vector<Parameter>;

    struct FunctionDefinition {
        Identifier name;
        ParameterList parameters;
        Type return_type;
        Block body;
    };

    using Program = std::vector<std::variant<FunctionDefinition>>;

    [[nodiscard]] Program parse(const Lexer::TokenList& tokens);

}// namespace Parser
