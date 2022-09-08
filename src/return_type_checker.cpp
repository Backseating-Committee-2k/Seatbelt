//
// Created by coder2k on 04.08.2022.
//

#include "return_type_checker.hpp"
#include "error.hpp"
#include <fmt/core.h>

void ReturnTypeChecker::visit(Parser::Statements::Block& statement) {
    usize i;
    bool ended_because_of_break_or_continue = false;
    for (i = 0; i < statement.statements.size(); ++i) {
        const auto& sub_statement = statement.statements[i];
        auto visitor = ReturnTypeChecker{};
        sub_statement->accept(visitor);
        if (visitor.ended_because_of_break_or_continue()) {
            ending_reason = visitor.ending_reason;
            ended_because_of_break_or_continue = true;
            break;
        }
        if (visitor.all_code_paths_return_a_value) {
            all_code_paths_return_a_value = true;
            break;
        }
    }
    if ((all_code_paths_return_a_value or ended_because_of_break_or_continue) and i < statement.statements.size() - 1) {
        Error::warning(*statement.statements[i + 1], "unreachable code detected");
    }
}

void ReturnTypeChecker::visit(Parser::Statements::IfStatement& statement) {
    auto then_visitor = ReturnTypeChecker{};
    statement.then_block.accept(then_visitor);
    if (then_visitor.ended_because_of_break_or_continue()) {
        ending_reason = then_visitor.ending_reason;
        return;
    }
    auto else_visitor = ReturnTypeChecker{};
    statement.else_block.accept(else_visitor);
    all_code_paths_return_a_value =
            then_visitor.all_code_paths_return_a_value and else_visitor.all_code_paths_return_a_value;
}

void ReturnTypeChecker::visit(Parser::Statements::LoopStatement& statement) {
    statement.body.accept(*this);
    if (ending_reason != EndingReason::Break) {
        // If the loop never terminates, we can pretend to give anything back because nobody can prove otherwise.
        // (blame: tomprogrammer, 2022-08-05)
        all_code_paths_return_a_value = true;
    }
    ending_reason = EndingReason::DidNotEnd;
}

void ReturnTypeChecker::visit(Parser::Statements::BreakStatement&) {
    ending_reason = EndingReason::Break;
}

void ReturnTypeChecker::visit(Parser::Statements::ContinueStatement&) {
    ending_reason = EndingReason::Continue;
}

void ReturnTypeChecker::visit(Parser::Statements::WhileStatement& statement) {
    statement.body.accept(*this);
    all_code_paths_return_a_value = false;
    ending_reason = EndingReason::DidNotEnd;
}

void ReturnTypeChecker::visit(Parser::Statements::DoWhileStatement& statement) {
    statement.body.accept(*this);
    ending_reason = EndingReason::DidNotEnd;
}

void ReturnTypeChecker::visit(Parser::Statements::ForStatement& statement) {
    statement.body.accept(*this);

    // a for-loop is considered to be infinite if there is no condition, e.g. for ;; { /* ... */ }
    const auto is_infinite_loop = not statement.condition;
    if (is_infinite_loop and ending_reason != EndingReason::Break) {
        // If the loop never terminates, we can pretend to give anything back because nobody can prove otherwise.
        // (see loop-statement)
        all_code_paths_return_a_value = true;
    } else {
        all_code_paths_return_a_value = false;
    }

    ending_reason = EndingReason::DidNotEnd;
}

void ReturnTypeChecker::visit(Parser::Statements::ReturnStatement&) {
    all_code_paths_return_a_value = true;
}

void ReturnTypeChecker::visit(Parser::Statements::VariableDefinition&) { }
void ReturnTypeChecker::visit(Parser::Statements::InlineAssembly&) { }
void ReturnTypeChecker::visit(Parser::Statements::ExpressionStatement&) { }
void ReturnTypeChecker::visit(Parser::Statements::LabelDefinition&) { }
void ReturnTypeChecker::visit(Parser::Statements::GotoStatement&) { }
