#include "lexer.hpp"
#include "parser.hpp"
#include "emitter.hpp"
#include "type_checker.hpp"
#include "scope_generator.hpp"
#include "type_container.hpp"
#include <argh.h>
#include <iostream>
#include <string>
#include <fstream>
#include <filesystem>

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
    std::ofstream stream{ filename, std::ios::out };
    stream << contents;
}

int main(int argc, char** argv) {
    using namespace Lexer::Tokens;

    auto command_line_parser = argh::parser{};
    command_line_parser.add_params({ "-o", "--output" });
    command_line_parser.parse(argc, argv);

    const auto [filename, source] = read_source_code(command_line_parser);
    const auto source_code = SourceCode{ .filename{ filename }, .text{ source } };
    const auto tokens = Lexer::tokenize(source_code);
    auto program = Parser::parse(tokens);

    auto type_container = TypeContainer{};
    ScopeGenerator::generate(program, type_container);
    TypeChecker::check(program, type_container);

    std::string assembly = "jump main\n\n";

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
