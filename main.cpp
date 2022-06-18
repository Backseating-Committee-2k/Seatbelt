#include "lexer.hpp"
#include "parser.hpp"
#include <iostream>
#include <string>
#include <fstream>
#include <filesystem>
#include <format>

[[nodiscard]] std::string read_whole_stream(std::istream& stream) {
    return { std::istreambuf_iterator<char>(stream), {} };
}

[[nodiscard]] std::tuple<std::string_view, std::string> read_source_code(int argc, char* const* argv) {
    if (argc < 2) {
        return std::tuple{ "<stdin>", read_whole_stream(std::cin) };
    } else if (argc == 2) {
        if (!std::filesystem::exists(argv[1])) {
            std::cerr << "file does not exist (" << argv[1] << ")\n";
            std::exit(EXIT_FAILURE);
        }
        std::ifstream stream{ argv[1], std::ios::in };
        return std::tuple{ argv[1], read_whole_stream(stream) };
    } else {
        std::cerr << "more than one argument is not supported as of yet\n";
        std::exit(EXIT_FAILURE);
    }
}

struct TokenPrinter {
    void operator()(const auto& token) {
        std::cout << token.debug_name << " (\"" << token.location.view() << "\")\n";
    }
};

void print_expression(const Parser::Expressions::Expression& expression) {
    using namespace Parser::Expressions;
    if (const auto literal_expression = dynamic_cast<const Literal*>(&expression)) {
        std::cout << literal_expression->value.location.view();
    } else if (const auto name_expression = dynamic_cast<const Name*>(&expression)) {
        std::cout << name_expression->name.location.view();
    } else if (const auto add_expression = dynamic_cast<const Addition*>(&expression)) {
        std::cout << "(";
        print_expression(*add_expression->lhs);
        std::cout << " + ";
        print_expression(*add_expression->rhs);
        std::cout << ")";
    } else if (const auto mult_expression = dynamic_cast<const Multiplication*>(&expression)) {
        std::cout << "(";
        print_expression(*mult_expression->lhs);
        std::cout << " * ";
        print_expression(*mult_expression->rhs);
        std::cout << ")";
    }
}

int main(int argc, char** argv) {
    using namespace Lexer::Tokens;

    const auto [filename, source] = read_source_code(argc, argv);
    const auto source_code = SourceCode{ .filename{ filename }, .text{ source } };
    const auto tokens = Lexer::tokenize(source_code);
    const auto program = Parser::parse(tokens);
    for (const auto& item : program) {
        std::visit(
                [](const Parser::FunctionDefinition& function_definition) {
                    std::cout << std::format("{}(", function_definition.name.location.view());
                    for (const auto& parameter : function_definition.parameters) {
                        std::cout << parameter.name.location.view() << ", ";
                    }
                    std::cout << ")\n";
                    for (const auto& statement : function_definition.body.statements) {
                        if (const auto variable_definition =
                                    dynamic_cast<const Parser::Statements::VariableDefinition*>(statement.get())) {
                            std::cout << std::format("\t{} = ", variable_definition->name.location.view());
                            print_expression(*variable_definition->initial_value);
                            std::cout << "\n";
                        }
                    }
                },
                item
        );
    }
    std::exit(EXIT_SUCCESS);
}
