#include "emitter.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "scope_generator.hpp"
#include "type_checker.hpp"
#include "type_container.hpp"
#include <argh.h>
#include <filesystem>
#include <fmt/core.h>
#include <fstream>
#include <iostream>
#include <string>

[[nodiscard]] std::string read_whole_stream(std::istream& stream) {
    return { std::istreambuf_iterator<char>(stream), {} };
}

[[nodiscard]] std::tuple<std::string, std::string> read_source_code(argh::parser& command_line_parser) {
    const usize num_arguments = command_line_parser.size();
    if (num_arguments < 2) {
        return std::tuple{ "<stdin>", read_whole_stream(std::cin) };
    } else if (num_arguments == 2) {
        if (!std::filesystem::exists(command_line_parser[1])) {
            std::cerr << "file does not exist (" << command_line_parser[1] << ")\n";
            std::exit(EXIT_FAILURE);
        }
        std::ifstream stream{ command_line_parser[1], std::ios::in };
        return std::tuple{ command_line_parser[1], read_whole_stream(stream) };
    } else {
        std::cerr << "more than one argument is not supported as of yet\n";
        std::exit(EXIT_FAILURE);
    }
}

void write_to_file(const std::string_view contents, const std::string_view filename) {
    std::ofstream stream{ std::string{ filename }, std::ios::out };
    stream << contents;
}

void check_main_function(const SourceCode& source_code, Scope& global_scope, TypeContainer& type_container) {
    const auto error = [source_code](const std::string_view message) {
        fmt::print(stderr, "{}:1:1: {}\n", source_code.filename, message);
        exit(EXIT_FAILURE);
    };
    const auto find_main_iterator = global_scope.find("main");
    const auto main_symbol_found = (find_main_iterator != std::cend(global_scope));
    if (not main_symbol_found) {
        error("no main function provided");
    }
    const auto main_symbol = std::get_if<FunctionSymbol>(&find_main_iterator->second);
    const auto main_function_found = (main_symbol != nullptr);
    if (not main_function_found) {
        error("no main function provided");
    }
    const auto exactly_one_main_function = (main_symbol->overloads.size() == 1);
    if (not exactly_one_main_function) {
        error("main function must not be overloaded");
    }
    const auto expected_main_function_return_type =
            type_container.from_data_type(std::make_unique<ConcreteType>("Void", false));
    const auto main_function_has_correct_signature =
            (main_symbol->overloads.front().signature == "$main" and
             main_symbol->overloads.front().return_type == expected_main_function_return_type);
    if (not main_function_has_correct_signature) {
        error("main function must not take any parameters and must return Void");
    }
}
int main(int argc, char** argv) {
    using namespace Lexer::Tokens;

    auto command_line_parser = argh::parser{};
    command_line_parser.add_params({ "-o", "--output" });
    command_line_parser.parse(argc, argv);

    const auto [filename, source] = read_source_code(command_line_parser);
    const auto source_code = SourceCode{ .filename{ filename }, .text{ source } };
    auto program = Parser::Program{};
    const auto tokens = Lexer::tokenize(source_code);
    program = Parser::parse(tokens);

    auto global_scope = Scope{ nullptr };
    auto type_container = TypeContainer{};
    ScopeGenerator::generate(program, type_container, global_scope);
    TypeChecker::check(program, type_container, global_scope);

    check_main_function(source_code, global_scope, type_container);

    std::string assembly = "jump $main\n";

    for (const auto& item : program) {
        assembly += std::visit(Emitter::Emitter{ &program }, item);
    }

    auto out_filename = std::string{};
    if (command_line_parser({ "-o", "--output" }) >> out_filename) {
        std::cout << "out filename is " << out_filename << "\n";
    }

    if (out_filename.empty()) {
        std::cout << assembly;
    } else {
        write_to_file(assembly, out_filename);
    }

    std::exit(EXIT_SUCCESS);
}
