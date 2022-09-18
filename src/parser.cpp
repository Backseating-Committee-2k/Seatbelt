//
// Created by coder2k on 17.06.2022.
//

#include "parser.hpp"
#include "error.hpp"
#include "namespace.hpp"
#include "type_container.hpp"
#include "types.hpp"
#include "utils.hpp"
#include <algorithm>
#include <cassert>
#include <fmt/core.h>
#include <fmt/format.h>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace Parser {

    template<std::integral T>
    struct ParsedNumber {
        T value;
        usize base;
    };

    using Expressions::Expression;

    class ParserState {
    public:
        ParserState(const Lexer::TokenList& tokens, TypeContainer* type_container, NamespacesMap previous_namespaces)
            : m_tokens{ &tokens },
              m_type_container{ type_container },
              m_namespaces{ std::move(previous_namespaces) } { }

        ParserState(const Lexer::TokenList& tokens, TypeContainer* type_container)
            : m_tokens{ &tokens },
              m_type_container{ type_container } { }

        [[nodiscard]] Program parse() {
            auto program = parse_header();
            concatenate_programs(program, parse_body());
            return program;
        }

        [[nodiscard]] Program parse_header() {
            auto program = Program{};
            while (not end_of_file() and current_is<Import>()) {
                program.push_back(import_statement());
            }
            return program;
        }

        [[nodiscard]] Program parse_body() {
            auto program = Program{};
            while (not end_of_file()) {
                if (current_is<Namespace>()) {
                    /* When a namespace is parsed but another namespace with the same name has been parsed before,
                     * the parse_namespace()-function does not create a new AST node, but instead concatenates the
                     * contents of the most recently parsed namespace onto the existing namespace. That's why the
                     * function returns an optional. If the optional has no value, we do not have to do anything. */
                    auto maybe_parsed = parse_namespace();
                    if (maybe_parsed) {
                        program.push_back(std::move(*maybe_parsed));
                    }
                } else if (current_is<Function>() or (current_is<Export>() and peek_is<Function>())) {
                    program.push_back(function_definition());
                } else if (current_is<Import>()) {
                    Error::error(current(), "imports must precede all other top level statements of a source file");
                } else {
                    break;
                }
            }
            return program;
        }

        [[nodiscard]] NamespacesMap&& move_out_namespaces_map() {
            return std::move(m_namespaces);
        }

    private:
        // this serves as a container for type tags
        template<typename... T>
        struct PrecedenceGroup { };

        [[nodiscard]] std::optional<std::unique_ptr<NamespaceDefinition>> parse_namespace() {
            using std::ranges::find_if;

            const auto namespace_token = consume<Namespace>();
            usize count = 1;
            const auto identifier = consume<Identifier>("expected identifier");
            m_namespaces_stack.emplace_back(identifier.location.view());
            while (maybe_consume<DoubleColon>()) {
                m_namespaces_stack.emplace_back(consume<Identifier>("expected identifier").location.view());
                ++count;
            }
            consume<LeftCurlyBracket>("expected \"{\"");

            const auto namespace_qualifier = get_namespace_qualifier(m_namespaces_stack);
            auto namespace_contents = parse_body();

            const auto find_iterator =
                    find_if(m_namespaces, [&](const auto& pair) { return pair.first == namespace_qualifier; });
            const auto found = (find_iterator != m_namespaces.cend());
            m_namespaces_stack.resize(m_namespaces_stack.size() - count);
            consume<RightCurlyBracket>("expected \"}\"");

            if (found) {
                const auto namespace_pointer = find_iterator->second;
                concatenate_programs(namespace_pointer->contents, std::move(namespace_contents));
                return {};
            } else {
                auto result = std::make_unique<NamespaceDefinition>(NamespaceDefinition{
                        .namespace_token{ namespace_token },
                        .name{ identifier },
                        .contents{ std::move(namespace_contents) },
                        .scope{} });
                m_namespaces[namespace_qualifier] = result.get();
                return result;
            }
        }

        [[nodiscard]] std::unique_ptr<Expression> expression() {
            return assignment();
        }

        [[nodiscard]] std::unique_ptr<Expression> assignment() {
            // clang-format off

            // left-associative binary operators, from lowest to highest precedence
            auto lhs = binary_operator(
                    PrecedenceGroup<Or>{},
                    PrecedenceGroup<And>{},
                    PrecedenceGroup<EqualsEquals, ExclamationEquals>{},
                    PrecedenceGroup<LessThan, LessOrEquals, GreaterThan, GreaterOrEquals>{},
                    PrecedenceGroup<Plus, Minus>{},
                    PrecedenceGroup<Asterisk, ForwardSlash, Mod>{}
            );
            // clang-format on

            if (const auto equals_token = maybe_consume<Equals>()) {
                auto rhs = expression();
                return std::make_unique<Parser::Expressions::Assignment>(
                        std::move(lhs), equals_token.value(), std::move(rhs)
                );
            } else {
                return lhs;
            }
        }

        template<typename FirstGroup, typename... RemainingGroups>
        [[nodiscard]] std::unique_ptr<Expression> binary_operator(FirstGroup first, RemainingGroups... remaining) {
            using Expressions::BinaryOperator;
            auto accumulator = binary_operator(remaining...); // fold expression leads to parsing the last group first
            while (auto current_token =
                           maybe_consume_one_of(first)) {          // try to consume any of the tokens of first group
                auto next_operand = binary_operator(remaining...); // same as above, but now for right-hand side

                // nest the parsed expressions to accomplish left-associativity
                accumulator = std::make_unique<BinaryOperator>(
                        std::move(accumulator), std::move(next_operand), current_token.value()
                );
            }
            return accumulator;
        }

        // When called without any arguments, this overload is chosen. Since there are no precedence groups left over,
        // we just pass on the call to the expression with the next higher precedence.
        [[nodiscard]] std::unique_ptr<Expression> binary_operator() {
            return right_associative_unary_operator();
        }

        [[nodiscard]] std::unique_ptr<Expression> right_associative_unary_operator() {
            if (current_is<Not>()) {
                const auto not_token = current();
                advance();
                return std::make_unique<Expressions::UnaryOperator>(not_token, right_associative_unary_operator());
            }
            if (current_is<At>()) {
                const auto at_token = current();
                advance();
                return std::make_unique<Expressions::UnaryOperator>(at_token, right_associative_unary_operator());
            }
            return function_call_or_index_operator_or_dereferencing();
        }

        [[nodiscard]] std::unique_ptr<Expression> function_call_or_index_operator_or_dereferencing() {
            using Expressions::FunctionCall, Expressions::BinaryOperator, Parser::IndexOperator;

            auto accumulator = primary();
            while (is_one_of<LeftParenthesis, LeftSquareBracket, ExclamationMark>(current())) {
                if (const auto left_parenthesis = maybe_consume<LeftParenthesis>()) {
                    std::vector<std::unique_ptr<Expression>> arguments;
                    while (not end_of_file() and not current_is<RightParenthesis>()) {
                        arguments.push_back(expression());
                        if (not maybe_consume<Comma>()) {
                            break;
                        }
                    }
                    consume<RightParenthesis>("expected \")\" at end of parameter list");
                    accumulator = std::make_unique<FunctionCall>(
                            std::move(accumulator), *left_parenthesis, std::move(arguments)
                    );
                } else if (maybe_consume<LeftSquareBracket>()) {
                    auto index = expression();
                    consume<RightSquareBracket>("expected \"]\"");
                    accumulator =
                            std::make_unique<BinaryOperator>(std::move(accumulator), std::move(index), IndexOperator{});
                } else if (current_is<ExclamationMark>()) {
                    const auto exclamation_mark_token = current();
                    advance();
                    accumulator = std::make_unique<Expressions::UnaryOperator>(
                            exclamation_mark_token, std::move(accumulator)
                    );
                } else {
                    assert(false and "unreachable");
                }
            }
            return accumulator;
        }

        [[nodiscard]] std::unique_ptr<Expression> array_literal() {
            auto left_square_bracket_token = consume<LeftSquareBracket>();
            auto first_value = expression();
            if (maybe_consume<Semicolon>()) {
                // array literal of the form [value; count]
                if (not current_is<IntegerLiteral>()) {
                    Error::error(current(), "expected number of array elements");
                }
                auto parsed_number = get_number_from_integer_literal<u32>(consume<IntegerLiteral>());
                consume<RightSquareBracket>("expected \"]\"");
                return std::make_unique<Expressions::ArrayLiteral>(
                        left_square_bracket_token, std::pair{ std::move(first_value), parsed_number.value }
                );
            } else {
                auto values = std::vector<std::unique_ptr<Expression>>{};
                values.push_back(std::move(first_value));
                while (not maybe_consume<RightSquareBracket>()) {
                    consume<Comma>("expected \",\"");
                    if (maybe_consume<RightSquareBracket>()) {
                        break;
                    }
                    values.push_back(expression());
                }
                return std::make_unique<Expressions::ArrayLiteral>(left_square_bracket_token, std::move(values));
            }
        }

        [[nodiscard]] std::unique_ptr<Expression> primary() {
            using namespace Expressions;

            if (current_is<LeftSquareBracket>()) {
                return array_literal();
            }
            if (const auto type_size_token = maybe_consume<TypeSize>()) {
                consume<LeftParenthesis>("expected \"(\"");
                const auto type_tokens_start = &current();
                auto type_definition = data_type();
                const auto type_tokens_end = &current();
                const auto type_tokens = std::span<const Token>{ type_tokens_start, type_tokens_end };
                consume<RightParenthesis>("expected \")\"");
                return std::make_unique<TypeSizeExpression>(*type_size_token, std::move(type_definition), type_tokens);
            }
            if (const auto value_size_token = maybe_consume<ValueSize>()) {
                consume<LeftParenthesis>("expected \"(\"");
                auto sub_expression = expression();
                consume<RightParenthesis>("expected \")\"");
                return std::make_unique<ValueSizeExpression>(*value_size_token, std::move(sub_expression));
            }
            if (maybe_consume<LeftParenthesis>()) {
                auto sub_expression = expression();
                consume<RightParenthesis>("expected \")\"");
                return sub_expression;
            }
            if (const auto integer_literal_token = maybe_consume<IntegerLiteral>()) {
                return std::make_unique<Integer>(integer_literal_token.value());
            }
            if (const auto char_literal_token = maybe_consume<CharLiteral>()) {
                return std::make_unique<Char>(char_literal_token.value());
            }
            if (const auto bool_literal_token = maybe_consume<BoolLiteral>()) {
                return std::make_unique<Bool>(bool_literal_token.value());
            }
            if (const auto nothing_literal_token = maybe_consume<NothingLiteral>()) {
                return std::make_unique<Nothing>(nothing_literal_token.value());
            }
            if (current_is<Identifier>()) {
                const usize name_start = m_index;
                advance();
                while (maybe_consume<DoubleColon>()) {
                    consume<Identifier>("expected identifier");
                }
                const usize name_end = m_index;
                return std::make_unique<Name>(std::span{ std::cbegin(*m_tokens) + name_start,
                                                         std::cbegin(*m_tokens) + name_end });
            }
            error("unexpected token");
            return nullptr;
        }

        [[nodiscard]] std::unique_ptr<FunctionDefinition> function_definition() {
            assert(current_is<Function>() or (current_is<Export>() and peek_is<Function>()));
            const std::optional<Export> export_token =
                    current_is<Export>() ? consume<Export>() : decltype(export_token){};
            assert(current_is<Function>());
            advance();
            const auto name = consume<Identifier>("expected function name");
            consume<LeftParenthesis>("expected \"(\"");

            // parameter list
            auto parameters = ParameterList{};
            while (not end_of_file() and not current_is<RightParenthesis>()) {
                const auto parameter_name = consume<Identifier>("expected parameter name");
                consume<Colon>("expected \":\"");
                const auto is_mutable = static_cast<bool>(maybe_consume<Mutable>());
                if (not is_mutable) {
                    maybe_consume<Const>();
                }
                const auto mutability = (is_mutable ? Mutability::Mutable : Mutability::Const);
                auto type_definition = data_type();
                parameters.push_back(Parameter{ .name{ parameter_name },
                                                .type_definition{ std::move(type_definition) },
                                                .binding_mutability{ mutability },
                                                .type{} });
                if (not maybe_consume<Comma>()) {
                    break;
                }
            }
            consume<RightParenthesis>("expected \")\"");
            std::unique_ptr<DataType> return_type_definition = std::make_unique<ConcreteType>(NothingIdentifier, true);
            auto return_type_tokens = std::span<const Token>{};
            if (maybe_consume<TildeArrow>()) {
                const auto return_type_definition_start = &current();
                return_type_definition = data_type();
                const auto return_type_definition_end = &current();
                return_type_tokens = std::span<const Token>{ return_type_definition_start, return_type_definition_end };
            } else if (not current_is<LeftCurlyBracket>()) {
                // this is irrelevant for the parsing logic, but it yields a better error message
                Error::error(
                        current(),
                        "expected either \"~>\" following a type definition, or a block if implicit return type "
                        "\"Nothing\" is intended"
                );
            }
            auto body = block();
            return std::make_unique<FunctionDefinition>(FunctionDefinition{
                    .name{ name },
                    .parameters{ std::move(parameters) },
                    .return_type_definition{ std::move(return_type_definition) },
                    .return_type_definition_tokens{ return_type_tokens },
                    .export_token{ export_token },
                    .return_type{},
                    .body{ std::move(body) } });
        }

        [[nodiscard]] std::unique_ptr<ImportStatement> import_statement() {
            assert(current_is<Import>());
            const auto import_token = consume<Import>("error should be impossible here");
            const usize path_start = m_index;
            consume<Identifier>("expected identifier");
            while (maybe_consume<Dot>()) {
                consume<Identifier>("expected identifier");
            }
            const usize path_end = m_index;
            consume_semicolon();
            return std::make_unique<ImportStatement>(ImportStatement{
                    .import_token{ import_token },
                    .import_path_tokens{ std::span{
                            std::begin(*m_tokens) + static_cast<Lexer::TokenList::difference_type>(path_start),
                            std::begin(*m_tokens) + static_cast<Lexer::TokenList::difference_type>(path_end),
                    } },
            });
        }

        [[nodiscard]] std::unique_ptr<Statements::IfStatement> if_statement() {
            const auto if_token = consume<If>();
            auto condition = expression();
            auto then_block = block();
            auto else_block = Statements::Block{ then_block.opening_bracket_token, Statements::StatementList{} };
            auto else_token = std::optional<Else>{};
            if ((else_token = maybe_consume<Else>())) {
                if (current_is<If>()) {
                    else_block.statements.push_back(if_statement());
                } else {
                    else_block = block();
                }
            }
            return std::make_unique<Statements::IfStatement>(
                    if_token, std::move(condition), std::move(then_block), else_token, std::move(else_block)
            );
        }

        [[nodiscard]] std::unique_ptr<Statements::LoopStatement> loop_statement() {
            const auto loop_token = consume<Loop>();
            return std::make_unique<Parser::Statements::LoopStatement>(loop_token, block());
        }

        [[nodiscard]] std::unique_ptr<Statements::BreakStatement> break_statement() {
            const auto loop_token = consume<Break>();
            consume_semicolon();
            return std::make_unique<Parser::Statements::BreakStatement>(loop_token);
        }

        [[nodiscard]] std::unique_ptr<Statements::ContinueStatement> continue_statement() {
            const auto continue_token = consume<Continue>();
            consume_semicolon();
            return std::make_unique<Parser::Statements::ContinueStatement>(continue_token);
        }

        [[nodiscard]] std::unique_ptr<Statements::WhileStatement> while_statement() {
            const auto while_token = consume<While>();
            auto condition = expression();
            auto body = block();
            return std::make_unique<Statements::WhileStatement>(while_token, std::move(condition), std::move(body));
        }

        [[nodiscard]] std::unique_ptr<Statements::DoWhileStatement> do_while_statement() {
            const auto do_token = consume<Do>();
            auto body = block();
            const auto while_token = consume<While>("expected \"while\"");
            auto condition = expression();
            consume_semicolon();
            return std::make_unique<Statements::DoWhileStatement>(
                    do_token, std::move(body), while_token, std::move(condition)
            );
        }

        [[nodiscard]] std::unique_ptr<Statements::ForStatement> for_statement() {
            const auto for_token = consume<For>();
            const auto uses_parentheses = maybe_consume<LeftParenthesis>();
            auto initializer = std::unique_ptr<Statements::Statement>{};
            if (current_is<Let>()) {
                initializer = variable_definition();
            } else if (not current_is<Semicolon>()) {
                initializer = std::make_unique<Statements::ExpressionStatement>(expression());
                consume_semicolon();
            } else {
                consume<Semicolon>();
            }

            auto condition = std::unique_ptr<Expression>{};
            if (not current_is<Semicolon>()) {
                condition = expression();
            }
            consume_semicolon();

            auto increment = std::unique_ptr<Expression>{};
            if ((uses_parentheses and not current_is<RightParenthesis>())
                or (not uses_parentheses and not current_is<LeftCurlyBracket>())) {
                increment = expression();
            }

            if (uses_parentheses) {
                consume<RightParenthesis>("expected \")\"");
            }

            auto body = block();
            return std::make_unique<Statements::ForStatement>(
                    for_token, std::move(initializer), std::move(condition), std::move(increment), std::move(body)
            );
        }

        [[nodiscard]] Statements::Block block() {
            Statements::StatementList statements;
            const auto opening_bracket_token = consume<LeftCurlyBracket>("expected \"{\"");
            while (not end_of_file() and not current_is<RightCurlyBracket>()) {
                if (current_is<If>()) {
                    statements.push_back(if_statement());
                } else if (current_is<Loop>()) {
                    statements.push_back(loop_statement());
                } else if (current_is<Break>()) {
                    statements.push_back(break_statement());
                } else if (current_is<Continue>()) {
                    statements.push_back(continue_statement());
                } else if (current_is<While>()) {
                    statements.push_back(while_statement());
                } else if (current_is<Do>()) {
                    statements.push_back(do_while_statement());
                } else if (current_is<For>()) {
                    statements.push_back(for_statement());
                } else if (current_is<Let>()) {
                    statements.push_back(variable_definition());
                } else if (current_is<Return>()) {
                    statements.push_back(return_statement());
                } else if (current_is<LeftCurlyBracket>()) {
                    statements.push_back(std::make_unique<Statements::Block>(block()));
                } else if (current_is<Label>()) {
                    statements.push_back(label_definition());
                } else if (current_is<Goto>()) {
                    statements.push_back(goto_statement());
                } else if (current_is<InlineAssembly>()) {
                    statements.push_back(std::make_unique<Statements::InlineAssembly>(std::get<InlineAssembly>(current()
                    )));
                    advance();
                } else {
                    auto expression = this->expression();
                    consume<Semicolon>("expected \";\" to complete expression statement");
                    statements.push_back(std::make_unique<Statements::ExpressionStatement>(std::move(expression)));
                }
            }
            consume<RightCurlyBracket>("expected \"}\"");
            return Statements::Block{ opening_bracket_token, std::move(statements) };
        }

        [[nodiscard]] std::unique_ptr<Statements::LabelDefinition> label_definition() {
            const auto label_token = consume<Label>();
            const auto identifier = consume<Identifier>("expected label identifier");
            consume_semicolon();
            return std::make_unique<Statements::LabelDefinition>(label_token, identifier);
        }

        [[nodiscard]] std::unique_ptr<Statements::GotoStatement> goto_statement() {
            const auto goto_token = consume<Goto>();
            const auto identifier = consume<Identifier>("expected identifier of jump target label");
            consume_semicolon();
            return std::make_unique<Statements::GotoStatement>(goto_token, identifier);
        }

        [[nodiscard]] std::unique_ptr<DataType> data_type() {
            if (current_is<LeftSquareBracket>()) {
                return array_type();
            } else if (current_is<Arrow>()) {
                return pointer_type();
            } else if (current_is<CapitalizedFunction>()) {
                return function_pointer_type();
            } else {
                return primitive_type();
            }
        }

        template<std::integral T>
        [[nodiscard]] static ParsedNumber<T> get_number_from_integer_literal(const IntegerLiteral& literal) {
            auto number_string = Utils::strip_underscores(literal.location.view());
            const auto base = Utils::get_base(literal.location.view());
            auto number_string_without_prefix =
                    (base == 10 ? std::string_view{ number_string } : std::string_view{ number_string }.substr(2));
            if (not Utils::validate_integer<T>(number_string_without_prefix, base)) {
                Error::error(literal, "integer literal out of bounds");
            }
            return ParsedNumber<T>{ .value{ Utils::parse_number<T>(number_string_without_prefix, base) },
                                    .base{ base } };
        }

        [[nodiscard]] std::unique_ptr<DataType> array_type() {
            consume<LeftSquareBracket>();
            auto contained = data_type();
            consume<Semicolon>("expected \";\"");
            auto num_elements_literal = consume<IntegerLiteral>("number of array elements expected");
            consume<RightSquareBracket>("expected \"]\"");

            const auto parsed_number = get_number_from_integer_literal<u32>(num_elements_literal);

            return std::make_unique<ArrayType>(
                    m_type_container->from_type_definition(std::move(contained)), parsed_number.value
            );
        }

        [[nodiscard]] std::unique_ptr<DataType> pointer_type() {
            consume<Arrow>();

            const auto is_mutable = static_cast<bool>(maybe_consume<Mutable>());
            if (not is_mutable) {
                maybe_consume<Const>();
            }
            const auto mutability = is_mutable ? Mutability::Mutable : Mutability::Const;

            auto pointee_type = data_type();
            return std::make_unique<PointerType>(
                    m_type_container->from_type_definition(std::move(pointee_type)), mutability
            );
        }

        [[nodiscard]] std::unique_ptr<DataType> function_pointer_type() {
            consume<CapitalizedFunction>();
            consume<LeftParenthesis>("expected \"(\"");
            auto parameter_types = std::vector<const DataType*>{};
            while (not end_of_file() and not current_is<RightParenthesis>()) {
                parameter_types.push_back(m_type_container->from_type_definition(data_type()));
                if (not maybe_consume<Comma>()) {
                    break;
                }
            }
            consume<RightParenthesis>("expected \")\" to terminate parameter list of function pointer type");
            consume<TildeArrow>("expected \"~>\"");
            auto return_type = data_type();
            return std::make_unique<FunctionPointerType>(
                    std::move(parameter_types), m_type_container->from_type_definition(std::move(return_type))
            );
        }

        [[nodiscard]] std::unique_ptr<DataType> primitive_type() {
            auto identifier = consume<Identifier>("type identifier expected");
            return std::make_unique<ConcreteType>(identifier.location.view(), false);
        }

        [[nodiscard]] std::unique_ptr<Statements::VariableDefinition> variable_definition() {
            const auto let_token = consume<Let>();
            const auto identifier = consume<Identifier>("expected variable name");
            auto type_definition = std::unique_ptr<DataType>{};
            auto is_mutable = false;
            auto type_tokens = std::span<const Token>{};
            if (not current_is<Equals>()) {
                consume<Colon>("expected \":\"");
                // either automatic type deduction with mutable binding, or annotated type
                is_mutable = static_cast<bool>(maybe_consume<Mutable>());
                if (not is_mutable) {
                    maybe_consume<Const>();
                }

                if (not current_is<Equals>()) {
                    // type is annotated
                    const auto type_tokens_start = &current();
                    type_definition = data_type();
                    const auto type_tokens_end = &current();
                    type_tokens = std::span<const Token>{ type_tokens_start, type_tokens_end };
                }
            }
            const auto mutability = (is_mutable ? Mutability::Mutable : Mutability::Const);
            auto equals_token = consume<Equals>("expected variable initialization");
            auto initial_value = expression();
            consume_semicolon();
            return std::make_unique<Statements::VariableDefinition>(
                    let_token, identifier, equals_token, type_tokens, std::move(type_definition),
                    std::move(initial_value), mutability
            );
        }

        [[nodiscard]] std::unique_ptr<Statements::ReturnStatement> return_statement() {
            const auto return_token = consume<Return>();
            auto return_value = std::unique_ptr<Expression>{};
            if (not current_is<Semicolon>()) {
                return_value = expression();
            }
            consume_semicolon();
            return std::make_unique<Statements::ReturnStatement>(return_token, std::move(return_value));
        }

        void consume_semicolon() {
            consume<Semicolon>("expected \";\"");
        }

        template<typename T>
        [[nodiscard]] bool current_is() const {
            return std::holds_alternative<T>(current());
        }

        template<typename T>
        [[nodiscard]] bool next_is() const {
            return std::holds_alternative<T>(peek());
        }

        template<typename T>
        T consume(const std::string_view message = "") {
            if (not current_is<T>()) {
                if (message.empty()) {
                    assert(false and "this error should be unreachable");
                }
                error(message);
            }
            const auto result = std::get<T>(current());
            advance();
            return result;
        }

        template<typename T>
        std::optional<T> maybe_consume() {
            if (current_is<T>()) {
                const auto result = std::get<T>(current());
                advance();
                return result;
            }
            return {};
        }

        template<typename FirstType, typename... RemainingTypes>
        std::optional<Token> maybe_consume_one_of() {
            if constexpr (sizeof...(RemainingTypes) == 0) {
                return maybe_consume<FirstType>();
            } else {
                auto result = maybe_consume<FirstType>();
                if (result) {
                    return result;
                }
                return maybe_consume_one_of<RemainingTypes...>();
            }
        }

        // This function "unpacks" the type tags that are contained in a PrecedenceGroup
        // and invokes its own overload (see above) with the unpacked types as
        // template type parameters.
        // Example: A call to maybe_consume_one_of(PrecedenceGroup<Plus, Minus>{}) leads to a call
        //          to maybe_consume_one_of<Plus, Minus>() (notice that the template type arguments
        //          in the first call are automatically deduced using the passed argument).
        template<typename... TokenTypes>
        std::optional<Token> maybe_consume_one_of(PrecedenceGroup<TokenTypes...>) {
            return maybe_consume_one_of<TokenTypes...>();
        }

        [[nodiscard]] const Token& current() const {
            return (*m_tokens)[m_index];
        }

        [[nodiscard]] const Token& peek() const {
            return (*m_tokens)[m_index + 1];
        }

        template<typename T>
        [[nodiscard]] bool peek_is() const {
            return std::holds_alternative<T>(peek());
        }

        [[nodiscard]] bool end_of_file() const {
            return m_index >= m_tokens->size() || current_is<EndOfFile>();
        }

        void advance() {
            ++m_index;
        }

        void error(const std::string_view message) const {
            Error::error(current(), message);
        }

    private:
        usize m_index{ 0 };
        const Lexer::TokenList* m_tokens;
        NamespacesStack m_namespaces_stack{};
        TypeContainer* m_type_container;
        NamespacesMap m_namespaces{};
    };

    void concatenate_programs(Program& first, Program&& second) {
        first.reserve(first.size() + second.size());
        for (auto& top_level_statement : second) {
            first.push_back(std::move(top_level_statement));
        }
    }

    [[nodiscard]] std::pair<Program, NamespacesMap>
    parse(const Lexer::TokenList& tokens, TypeContainer& type_container) {
        return parse(tokens, type_container, NamespacesMap{});
    }

    [[nodiscard]] std::pair<Program, NamespacesMap>
    parse(const Lexer::TokenList& tokens, TypeContainer& type_container, NamespacesMap previous_namespaces) {
        auto parser_state = ParserState{ tokens, &type_container, std::move(previous_namespaces) };
        auto program = parser_state.parse();
        auto namespaces_map = std::move(parser_state.move_out_namespaces_map());
        return { std::move(program), std::move(namespaces_map) };
    }

} // namespace Parser
