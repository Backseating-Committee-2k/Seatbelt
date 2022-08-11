//
// Created by coder2k on 24.06.2022.
//

#include "error.hpp"
#include "parser.hpp"
#include <algorithm>
#include <fmt/core.h>
#include <iostream>
#include <ranges>
#include <stdexcept>

struct GetFirstTokenVisitor : public Parser::Statements::StatementVisitor,
                              public Parser::Expressions::ExpressionVisitor {
    void visit(Parser::Statements::Block& statement) override {
        token = statement.opening_bracket_token;
    }

    void visit(Parser::Statements::IfStatement& statement) override {
        token = statement.if_token;
    }

    void visit(Parser::Statements::LoopStatement& statement) override {
        token = statement.loop_token;
    }

    void visit(Parser::Statements::BreakStatement& statement) override {
        token = statement.break_token;
    }

    void visit(Parser::Statements::ContinueStatement& statement) override {
        token = statement.continue_token;
    }

    void visit(Parser::Statements::WhileStatement& statement) override {
        token = statement.while_token;
    }

    void visit(Parser::Statements::DoWhileStatement& statement) override {
        token = statement.do_token;
    }

    void visit(Parser::Statements::ForStatement& statement) override {
        token = statement.for_token;
    }

    void visit(Parser::Statements::ReturnStatement& statement) override {
        token = statement.return_token;
    }

    void visit(Parser::Statements::VariableDefinition& statement) override {
        token = statement.let_token;
    }

    void visit(Parser::Statements::InlineAssembly& statement) override {
        token = statement.token;
    }

    void visit(Parser::Statements::ExpressionStatement& statement) override {
        statement.expression->accept(*this);
    }

    void visit(Parser::Statements::LabelDefinition& statement) override {
        token = statement.label_token;
    }

    void visit(Parser::Statements::GotoStatement& statement) override {
        token = statement.goto_token;
    }

    void visit(Parser::Expressions::Integer& expression) override {
        token = expression.value;
    }

    void visit(Parser::Expressions::Char& expression) override {
        token = expression.value;
    }

    void visit(Parser::Expressions::Bool& expression) override {
        token = expression.value;
    }

    void visit(Parser::Expressions::Name& expression) override {
        assert(not expression.name_tokens.empty() and "a name cannot consist of zero tokens");
        token = expression.name_tokens.front();
    }

    void visit(Parser::Expressions::BinaryOperator& expression) override {
        expression.lhs->accept(*this);
    }

    void visit(Parser::Expressions::FunctionCall& expression) override {
        expression.callee->accept(*this);
    }

    void visit(Parser::Expressions::Assignment& expression) override {
        expression.assignee->accept(*this);
    }

    void visit(Parser::Expressions::Nothing& expression) override {
        token = expression.nothing_token;
    }

    Lexer::Tokens::Token token;
};

void print_message(const Lexer::Tokens::Token& token, const std::string_view message) {
    using namespace std::ranges::views;
    using std::ranges::count, std::ranges::find;
    const auto npos = std::string_view::npos;

    const auto location = Error::token_location(token);
    const auto row = count(location.source_code.text | take(location.offset_start_inclusive), '\n') + 1;
    const auto last_newline_pos = location.source_code.text.find_last_of('\n', location.offset_start_inclusive);
    const auto column = location.offset_start_inclusive - (last_newline_pos == npos ? -1 : last_newline_pos);

    const auto line_start_pos = last_newline_pos == npos ? 0 : last_newline_pos + 1;
    const auto next_newline_pos = location.source_code.text.find('\n', line_start_pos);
    const auto line_end_pos = next_newline_pos == npos ? location.source_code.text.length() : next_newline_pos;
    const auto line = location.source_code.text.substr(line_start_pos, line_end_pos - line_start_pos);

    std::cerr << fmt::format("{}:{}:{}: {}\n{}\n", location.source_code.filename, row, column, message, line);
    for (usize i = 0; i < column - 1; ++i) {
        std::cerr << ' ';
    }
    std::cerr << '^';
    const auto squiggly_length = std::min(std::max(location.view().length(), usize{ 1 }) - 1, line.length() - column);
    for (usize i = 0; i < squiggly_length; ++i) {
        std::cerr << '~';
    }
}

[[nodiscard]] Lexer::Tokens::Token get_first_token(const Parser::Statements::Statement& statement) {
    auto visitor = GetFirstTokenVisitor{};

    // We cast away the const of the statement because StatementVisitor-instances require the argument
    // to be mutable. However, our visitor does not mutate anything and thus this is safe (don't quote me on
    // that, though).
    const_cast<Parser::Statements::Statement&>(statement).accept(visitor);
    return visitor.token;
}

[[nodiscard]] Lexer::Tokens::Token get_first_token(const Parser::Expressions::Expression& statement) {
    auto visitor = GetFirstTokenVisitor{};

    // We cast away the const of the statement because StatementVisitor-instances require the argument
    // to be mutable. However, our visitor does not mutate anything and thus this is safe (don't quote me on
    // that, though).
    const_cast<Parser::Expressions::Expression&>(statement).accept(visitor);
    return visitor.token;
}

namespace Error {

    void Error::error(const Lexer::Tokens::Token& token, const std::string_view message) {
        print_message(token, message);
        std::cerr << " error occurred here\n";
        // throw std::runtime_error{ std::string{ message } };
        std::exit(EXIT_FAILURE);
    }

    void warning(const Lexer::Tokens::Token& token, const std::string_view message) {
        print_message(token, fmt::format("warning: {}", message));
        std::cerr << " see here\n";
    }

    void error(const Parser::Statements::Statement& statement, std::string_view message) {
        Error::error(get_first_token(statement), message);
    }

    void warning(const Parser::Statements::Statement& statement, std::string_view message) {
        Error::warning(get_first_token(statement), message);
    }

    void error(const Parser::Expressions::Expression& expression, std::string_view message) {
        Error::error(get_first_token(expression), message);
    }

    void warning(const Parser::Expressions::Expression& expression, std::string_view message) {
        Error::warning(get_first_token(expression), message);
    }
}// namespace Error
