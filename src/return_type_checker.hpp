//
// Created by coder2k on 04.08.2022.
//

#pragma once

#include "data_type.hpp"
#include "parser.hpp"
#include <variant>

struct ReturnTypeChecker : public Parser::Statements::StatementVisitor {
    void visit(Parser::Statements::Block& statement) override;
    void visit(Parser::Statements::IfStatement& statement) override;
    void visit(Parser::Statements::LoopStatement& statement) override;
    void visit(Parser::Statements::BreakStatement& statement) override;
    void visit(Parser::Statements::ContinueStatement& statement) override;
    void visit(Parser::Statements::WhileStatement& statement) override;
    void visit(Parser::Statements::DoWhileStatement& statement) override;
    void visit(Parser::Statements::ForStatement& statement) override;
    void visit(Parser::Statements::ReturnStatement& statement) override;
    void visit(Parser::Statements::VariableDefinition& statement) override;
    void visit(Parser::Statements::InlineAssembly& statement) override;
    void visit(Parser::Statements::ExpressionStatement& statement) override;
    void visit(Parser::Statements::LabelDefinition& statement) override;
    void visit(Parser::Statements::GotoStatement& statement) override;

    enum class EndingReason {
        DidNotEnd,
        Break,
        Continue,
    };

    [[nodiscard]] bool ended_because_of_break_or_continue() const {
        return ending_reason == EndingReason::Break or ending_reason == EndingReason::Continue;
    }

    bool all_code_paths_return_a_value{ false };
    EndingReason ending_reason{ EndingReason::DidNotEnd };
};
