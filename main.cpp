#include "emitter.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "scope_generator.hpp"
#include "type_checker.hpp"
#include "type_container.hpp"
#include <algorithm>
#include <argh.h>
#include <filesystem>
#include <fmt/core.h>
#include <fstream>
#include <iostream>
#include <ranges>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <variant>

[[nodiscard]] std::string read_whole_stream(std::istream& stream) {
    return { std::istreambuf_iterator<char>(stream), {} };
}

[[nodiscard]] std::pair<std::string, std::string> read_source_code(argh::parser& command_line_parser) {
    const usize num_arguments = command_line_parser.size();
    if (num_arguments < 2) {
        return { "<stdin>", read_whole_stream(std::cin) };
    } else if (num_arguments == 2) {
        if (!std::filesystem::exists(command_line_parser[1])) {
            std::cerr << "file does not exist (" << command_line_parser[1] << ")\n";
            std::exit(EXIT_FAILURE);
        }
        std::ifstream stream{ command_line_parser[1], std::ios::in };
        const auto absolute_path = std::filesystem::current_path() / command_line_parser[1];
        return { absolute_path.string(), read_whole_stream(stream) };
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
            type_container.from_type_definition(std::make_unique<ConcreteType>("Void", false));
    const auto main_function_has_correct_signature =
            (main_symbol->overloads.front().signature == "$main" and
             main_symbol->overloads.front().return_type == expected_main_function_return_type);
    if (not main_function_has_correct_signature) {
        error("main function must not take any parameters and must return Void");
    }
    main_symbol->overloads.front().definition->is_entry_point = true;
}

enum class ImportStatus {
    Imported,
    NotImported,
};

struct ImportTask {
    ImportStatus status;
    std::optional<Lexer::Tokens::Token> token;
};

using ImportsMap = std::unordered_map<std::string, ImportTask>;
using SourceFileContainer = std::vector<std::pair<std::string, std::string>>;
using TokenListsContainer = std::vector<Lexer::TokenList>;

template<typename T>
struct PrintType;

std::unordered_map<std::string, Lexer::Tokens::Token>
collect_imports(const Parser::Program& program, const std::filesystem::path& base_directory) {
    using std::ranges::for_each;
    auto imports = std::unordered_map<std::string, Lexer::Tokens::Token>{};
    for_each(program, [&](const auto& top_level_statement) {
        std::visit(
                overloaded{
                        [&](const std::unique_ptr<Parser::ImportStatement>& import_statement) {
                            auto path = base_directory;
                            for (const auto& token : import_statement->import_path_tokens) {
                                if (const auto& identifier = std::get_if<Lexer::Tokens::Identifier>(&token)) {
                                    path = path / identifier->location.view();
                                } else if (not std::holds_alternative<Lexer::Tokens::Dot>(token)) {
                                    assert(false and "unreachable");
                                }
                            }
                            path += ".bs";

                            const auto path_string = path.string();
                            const auto last_token = import_statement->import_path_tokens.back();
                            if (imports.contains(path_string)) {
                                Error::warning(last_token, "duplicate import");
                            } else {
                                imports[path_string] = last_token;
                            }
                        },
                        [&](auto&&) {},
                },
                top_level_statement
        );
    });
    return imports;
}

[[nodiscard]] Parser::Program resolve_imports(
        argh::parser& command_line_parser,
        SourceFileContainer& source_files,
        TokenListsContainer& token_lists
) {
    using Parser::concatenate_programs;
    using std::ranges::find_if;

    // the empty string represents the main code file
    auto imports = ImportsMap{
        {"",
         ImportTask{
         .status{ ImportStatus::NotImported },
         .token{},
         }},
    };

    auto program = Parser::Program{};

    while (true) {
        const auto find_iterator =
                find_if(imports, [](const auto& pair) { return pair.second.status == ImportStatus::NotImported; });
        const auto found = (find_iterator != std::cend(imports));
        if (not found) {
            break;
        }

        const auto& path_string = find_iterator->first;
        auto& import_task = find_iterator->second;

        const auto is_main_file = path_string.empty();
        if (is_main_file) {
            source_files.push_back(read_source_code(command_line_parser));
        } else {
            const auto path = std::filesystem::path{ path_string };
            if (not exists(path)) {
                Error::error(import_task.token.value(), fmt::format("imported file not found: \"{}\"", path_string));
            }
            auto input_stream = std::ifstream{ path };
            if (not input_stream) {
                fmt::print(stderr, "unable to open file \"{}\"\n", path_string);
                std::exit(EXIT_FAILURE);
            }
            source_files.emplace_back(path_string, read_whole_stream(input_stream));
        }

        const auto& current_source_file = source_files.back();

        const auto base_directory = source_files.back().first == "<stdin>"
                                            ? std::filesystem::current_path()
                                            : std::filesystem::path{ current_source_file.first }.remove_filename();

        const auto& current_token_list = token_lists.emplace_back(Lexer::tokenize(SourceCode{
                .filename{ current_source_file.first },
                .text{ current_source_file.second },
        }));
        auto current_program = Parser::parse(current_token_list);

        if (is_main_file) {
            imports.erase("");
            imports[current_source_file.first] = ImportTask{ .status = ImportStatus::Imported, .token{} };
        }

        const auto current_imports = collect_imports(current_program, base_directory);
        for (const auto& current_import : current_imports) {
            if (imports.contains(current_import.first)) {
                if (current_import.first == current_source_file.first) {
                    Error::warning(current_import.second, "self-import has no effect");
                }
                continue;
            }
            imports[current_import.first] = ImportTask{
                .status{ ImportStatus::NotImported },
                .token{ current_import.second },
            };
        }

        concatenate_programs(program, std::move(current_program));

        if (not is_main_file) {
            import_task.status = ImportStatus::Imported;
        }
    }
    return program;
}

int main(int argc, char** argv) {
    using namespace Lexer::Tokens;

    auto command_line_parser = argh::parser{};
    command_line_parser.add_params({ "-o", "--output" });
    command_line_parser.parse(argc, argv);

    auto source_files = SourceFileContainer{};
    auto token_lists = TokenListsContainer{};

    auto program = resolve_imports(command_line_parser, source_files, token_lists);

    auto global_scope = Scope{ nullptr, "" };
    auto type_container = TypeContainer{};
    ScopeGenerator::generate(program, type_container, global_scope);
    TypeChecker::check(program, type_container, global_scope);

    check_main_function(
            SourceCode{
                    .filename{ source_files.front().first },
                    .text{ source_files.front().second },
            },
            global_scope, type_container
    );

    std::string assembly = "jump $main\n";

    for (const auto& item : program) {
        assembly += std::visit(Emitter::Emitter{ &program }, item);
    }

    auto out_filename = std::string{};
    if (command_line_parser({ "-o", "--output" }) >> out_filename) {
        write_to_file(assembly, out_filename);
    } else {
        fmt::print("{}", assembly);
    }

    std::exit(EXIT_SUCCESS);
}
