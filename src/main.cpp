#include "bssembly.hpp"
#include "bssembly_optimizer.hpp"
#include "emitter.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "magic_enum_wrapper.hpp"
#include "parser.hpp"
#include "scope_generator.hpp"
#include "stack_layout_generator.hpp"
#include "type_checker.hpp"
#include "type_container.hpp"
#include "upholsterer.hpp"
#include <algorithm>
#include <arguably.hpp>
#include <array>
#include <filesystem>
#include <fmt/core.h>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <ranges>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <upholsterer2k/upholsterer2k.h>
#include <variant>

#define VERSION "0.1.0"

[[nodiscard]] std::string read_whole_stream(std::istream& stream) {
    return { std::istreambuf_iterator<char>(stream), {} };
}

[[nodiscard]] std::pair<std::string, std::string> read_source_code(auto& command_line_parser) {
    if (not command_line_parser.template was_provided<'i'>()) {
        return { "<stdin>", read_whole_stream(std::cin) };
    } else {
        auto input_filename = command_line_parser.template get<'i'>();
        if (!std::filesystem::exists(input_filename)) {
            std::cerr << "file does not exist (" << input_filename << ")\n";
            std::exit(EXIT_FAILURE);
        }
        std::ifstream stream{ input_filename, std::ios::in };
        const auto absolute_path = std::filesystem::current_path() / input_filename;
        return { absolute_path.string(), read_whole_stream(stream) };
    }
}

void write_to_file(const std::string_view contents, const std::filesystem::path& filename) {
    std::ofstream stream{ filename, std::ios::out };
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
    const auto expected_main_function_return_type = type_container.get_nothing();
    const auto main_function_has_correct_signature =
            (main_symbol->overloads.front().signature == "main()"
             and main_symbol->overloads.front().definition->return_type == expected_main_function_return_type);
    if (not main_function_has_correct_signature) {
        error(fmt::format(
                "main function must not take any parameters and must return {}",
                magic_enum::enum_name(BasicType::Nothing)
        ));
    }
    main_symbol->overloads.front().definition->is_entry_point = true;
}

enum class ImportStatus {
    Imported,
    NotImported,
};

struct ImportTask {
    ImportStatus status;
    std::optional<Parser::ImportStatement> import_statement;
};

using ImportsMap = std::unordered_map<std::string, ImportTask>;
using SourceFileContainer = std::vector<std::pair<std::string, std::string>>;
using TokenListsContainer = std::vector<Lexer::TokenList>;

[[nodiscard]] std::filesystem::path get_relative_import_path(const std::span<const Lexer::Tokens::Token> tokens) {
    auto path = std::filesystem::path{};
    for (const auto& token : tokens) {
        if (const auto& identifier = std::get_if<Lexer::Tokens::Identifier>(&token)) {
            path = path / identifier->location.view();
        } else if (not std::holds_alternative<Lexer::Tokens::Dot>(token)) {
            assert(false and "unreachable");
        }
    }
    path += ".bs";
    return path;
}

std::unordered_map<std::string, Parser::ImportStatement>
collect_imports(const Parser::Program& program, const std::filesystem::path& base_directory) {
    using std::ranges::for_each;
    auto imports = std::unordered_map<std::string, Parser::ImportStatement>{};
    for_each(program, [&](const auto& top_level_statement) {
        std::visit(
                overloaded{
                        [&](const std::unique_ptr<Parser::ImportStatement>& import_statement) {
                            auto path = base_directory / get_relative_import_path(import_statement->import_path_tokens);

                            const auto path_string = path.string();
                            const auto last_token = import_statement->import_path_tokens.back();
                            if (imports.contains(path_string)) {
                                Error::warning(last_token, "duplicate import");
                            } else {
                                imports[path_string] = *import_statement;
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
        auto& command_line_parser,
        SourceFileContainer& source_files,
        TokenListsContainer& token_lists,
        TypeContainer& type_container,
        const std::vector<std::string>& import_paths
) {
    using Parser::concatenate_programs;
    using std::ranges::find_if;

    // the empty string represents the main code file
    auto imports = ImportsMap{
        {"",
         ImportTask{
         .status{ ImportStatus::NotImported },
         .import_statement{},
         }},
    };

    auto program = Parser::Program{};

    auto known_namespaces = Parser::NamespacesMap{};

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
        auto path = std::filesystem::path{ path_string };
        if (is_main_file) {
            source_files.push_back(read_source_code(command_line_parser));
        } else {
            auto import_path_iterator = import_paths.cbegin();
            const auto relative_path = get_relative_import_path((*import_task.import_statement).import_path_tokens);
            while (not exists(path)) {
                // we now try to fall back to the passed additional import paths

                if (import_path_iterator == import_paths.cend()) {
                    // even with fallback we couldn't find the file to import
                    Error::error(
                            import_task.import_statement.value().import_token,
                            fmt::format("imported file not found: \"{}\"", path_string)
                    );
                }
                path = std::filesystem::path{ *import_path_iterator } / relative_path;
                ++import_path_iterator;
            }

            auto input_stream = std::ifstream{ path };
            if (not input_stream) {
                fmt::print(stderr, "unable to open file \"{}\"\n", path_string);
                std::exit(EXIT_FAILURE);
            }
            source_files.emplace_back(path_string, read_whole_stream(input_stream));
        }

        const auto& current_source_file = source_files.back();

        const auto base_directory =
                source_files.back().first == "<stdin>"
                        ? std::filesystem::current_path()
                        : (is_main_file ? std::filesystem::path{ current_source_file.first }.remove_filename()
                                        : absolute(path).remove_filename());

        const auto& current_token_list = token_lists.emplace_back(Lexer::tokenize(SourceCode{
                .filename{ current_source_file.first },
                .text{ current_source_file.second },
        }));
        auto&& [current_program, namespaces] =
                Parser::parse(current_token_list, type_container, std::move(known_namespaces));

        static constexpr auto prelude_include_tokens = std::array<Lexer::Tokens::Token, 3>{
            Lexer::Tokens::token_prototype<Lexer::Tokens::Identifier>("std"),
            Lexer::Tokens::token_prototype<Lexer::Tokens::Dot>(),
            Lexer::Tokens::token_prototype<Lexer::Tokens::Identifier>("prelude"),
        };
        if (is_main_file) {
            current_program.push_back(std::make_unique<Parser::ImportStatement>(Parser::ImportStatement{
                    .import_token{ Lexer::Tokens::token_prototype<Lexer::Tokens::Import>() },
                    .import_path_tokens{ std::span{ prelude_include_tokens.begin(), prelude_include_tokens.end() } },
            }));
        }

        known_namespaces = std::move(namespaces);

        if (is_main_file) {
            imports.erase("");
            imports[current_source_file.first] = ImportTask{ .status = ImportStatus::Imported, .import_statement{} };
        }

        const auto current_imports = collect_imports(current_program, base_directory);
        for (const auto& current_import : current_imports) {
            if (imports.contains(current_import.first)) {
                if (current_import.first == current_source_file.first) {
                    Error::warning(current_import.second.import_token, "self-import has no effect");
                }
                continue;
            }
            imports[current_import.first] = ImportTask{
                .status{ ImportStatus::NotImported },
                .import_statement{ current_import.second },
            };
        }

        concatenate_programs(program, std::move(current_program));

        if (not is_main_file) {
            import_task.status = ImportStatus::Imported;
        }
    }
    return program;
}

[[nodiscard]] std::string_view trim(const std::string_view view) {
    bool found = false;
    usize left_index = 0;
    for (usize i = 0; i < view.length(); ++i) {
        if (not std::isspace(view[i])) {
            found = true;
            left_index = i;
            break;
        }
    }
    if (not found) {
        return "";
    }
    // the string_view MUST contain a character that is not a whitespace
    for (usize i = view.length() - 1;; --i) {
        if (not std::isspace(view[i])) {
            const usize right_index = i;
            return std::string_view{ &view[left_index], right_index - left_index + 1 };
        }
    }
    // unreachable
}

int main(int, char** argv) {
    using namespace Lexer::Tokens;
    using namespace std::string_view_literals;

    auto command_line_parser =
            arguably::create_parser()
                    .info<"Seatbelt v" VERSION " - official compiler for the Backseat-Safe System 2k">()
                    .help<"usage:">()
                    .named<'o', "output", "set the output filename", std::string>("-")
                    .optionally_named<'i', "input", "set the input filename", std::string>("-")
                    .named<'l', "lib", "pass a comma-separated list of paths to use for import-lookup", std::string>("")
                    .named<'m', "mapping-table",
                           "outputs a JSON file containing a mapping between Backseat code and Bssembler instructions "
                           "into the specified file",
                           std::string>("")
                    .flag<'b', "bssembly", "only output the compilation result without running the bssembler">()
                    .flag<'O', "optimize", "enable the optimizer">()
                    .flag<'v', "verbose", "show verbose output">()
                    .create();

    command_line_parser.parse(argv);
    if (not command_line_parser) {
        fmt::print(stderr, "error: erroneous command line arguments\n");
        fmt::print(stderr, "use --help to get information on the usage of the program\n");
        std::exit(EXIT_FAILURE);
    }

    if (command_line_parser.get<'h'>()) {
        command_line_parser.print_help(stdout);
        std::exit(EXIT_SUCCESS);
    }

    std::vector<std::string> import_paths;
    if (command_line_parser.was_provided<'l'>()) {
        using std::ranges::views::split, std::ranges::views::transform;
        using namespace std::string_view_literals;
        const auto import_paths_string = command_line_parser.get<'l'>();
        const auto paths_view = std::string_view{ import_paths_string };
        for (const auto path_view :
             split(paths_view, ";"sv) | transform([](auto view) { return trim(std::string_view{ view }); })) {
            import_paths.emplace_back(path_view);
        }
    }

    auto source_files = SourceFileContainer{};
    auto token_lists = TokenListsContainer{};
    auto type_container = TypeContainer{};

    auto program = resolve_imports(command_line_parser, source_files, token_lists, type_container, import_paths);

    const auto create_location = [](const std::string_view text) {
        return Location{
            .source_code{ .filename{}, .text{ text } },
            .offset_start_inclusive{ 0 },
            .offset_end_exclusive{ text.length() }
        };
    };

    auto global_namespace = std::make_unique<Parser::NamespaceDefinition>(Parser::NamespaceDefinition{
            .namespace_token{ .location{ create_location("namespace"sv) } },
            .name{ .location{ create_location(""sv) } },
            .contents{ std::move(program) },
            .scope{ std::make_unique<Scope>(nullptr, nullptr) } });
    global_namespace->scope->surrounding_namespace = global_namespace.get();

    program = Parser::Program{};
    program.push_back(std::move(global_namespace));

    assert(program.size() == 1 and "the program must exactly contain one namespace");
    assert(std::holds_alternative<std::unique_ptr<Parser::NamespaceDefinition>>(program.front())
           and "this must be a namespace");
    assert(std::get<std::unique_ptr<Parser::NamespaceDefinition>>(program.front())->name.location.view().empty()
           and "the global namespace must have an empty string view as name");
    assert(std::get<std::unique_ptr<Parser::NamespaceDefinition>>(program.front())->scope.get() != nullptr
           and "the global namespace must own the global scope");

    auto& global_scope = *std::get<std::unique_ptr<Parser::NamespaceDefinition>>(program.front())->scope;

    ScopeGenerator::generate(program, type_container, global_scope);
    TypeChecker::check(program, type_container, global_scope);
    StackLayoutGenerator::generate_stack_layout(program);

    check_main_function(
            SourceCode{
                    .filename{ source_files.front().first },
                    .text{ source_files.front().second },
            },
            global_scope, type_container
    );

    auto bssembly = Bssembler::Bssembly{};
    using namespace std::string_view_literals;
    bssembly.add(Bssembler::Instruction{ Bssembler::Mnemonic::CALL, { Bssembler::Immediate{ "$\"::main()\""sv } } });
    bssembly.add(Bssembler::Instruction{ Bssembler::Mnemonic::HALT, {} });

    auto label_generator = Emitter::LabelGenerator{};
    for (const auto& item : program) {
        bssembly += std::visit(Emitter::Emitter{ &program, &label_generator, &type_container }, item);
    }

    if (command_line_parser.get<'O'>()) {
        const auto size_before = bssembly.size();

        optimize(bssembly, command_line_parser.get<'v'>());

        const auto size_after = bssembly.size();
        if (command_line_parser.get<'v'>()) {
            fmt::print(
                    stderr, "optimization: {:.2} % ({} instructions to {} instructions)\n",
                    static_cast<double>(size_before - size_after) / static_cast<double>(size_before) * 100.0,
                    size_before, size_after, bssembly.size()
            );
        }
    } else if (command_line_parser.get<'v'>()) {
        fmt::print(stderr, "optimizations were disabled\n");
    }

    const auto bssembly_string = bssembly.to_string();
    const auto bssembly_view = UP2K_string_view_from_string(bssembly_string.c_str());
    const auto source_file =
            UP2K_SourceFile{ .filename{ UP2K_string_view_from_string("filename") }, .source{ bssembly_view } };
    UP2K_ByteVector machine_code;
    char error_message_buffer[512];
    const std::size_t error_message_buffer_size = std::size(error_message_buffer);
    if (not command_line_parser.get<'b'>()) {
        [[maybe_unused]] const auto result = UP2K_bssemble(
                source_file, nullptr, &machine_code, nullptr, error_message_buffer, error_message_buffer_size
        );
        assert(result and "not implemented");
    } else {
        machine_code = UP2K_byte_vector_create();
    }

    auto out_path = std::optional<std::filesystem::path>{};
    if (command_line_parser.was_provided<'o'>()) {
        auto out_filename = command_line_parser.get<'o'>();
        out_path = std::filesystem::path{ out_filename };
    }
    if (command_line_parser.get<'b'>()) {
        // only output the bssembler result
        if (out_path.has_value()) {
            write_to_file(bssembly.to_string(), *out_path);
        } else {
            fmt::print("{}", bssembly.to_string());
        }
    } else {
        // output the machine code
        if (not upholsterer2k::write_machine_code_to_file(machine_code, out_path)) {
            fmt::print(stderr, "unable to write machine code to file\n");
        }
    }

    UP2K_byte_vector_free(&machine_code);

    if (command_line_parser.was_provided<'m'>()) {
        const auto out_filename = command_line_parser.get<'m'>();
        auto mapping_table = std::string{ "{\n" };
        for (usize i = 0; i < bssembly.size(); ++i) {
            const auto instruction = std::get_if<Bssembler::Instruction>(&(bssembly[i]));
            if (instruction == nullptr or not instruction->origin_location.has_value()) {
                continue;
            }
            const auto origin_location = *(instruction->origin_location);
            mapping_table += fmt::format("\t\"{}\": {{\n", i + 1);
            mapping_table += fmt::format(
                    "\t\t\"backseat_filename\": {},\n", fmt::streamed(std::quoted(origin_location.source_code.filename))
            );
            const auto [row, column] = Error::row_and_column(origin_location);
            mapping_table += fmt::format("\t\t\"backseat_line\": {},\n", row);
            mapping_table += fmt::format("\t\t\"backseat_column_start\": {},\n", column);
            mapping_table += fmt::format("\t\t\"backseat_column_end\": {}\n", column + origin_location.view().length());
            mapping_table += "\t}";
            if (i < bssembly.size() - 1) {
                mapping_table += ",";
            }
            mapping_table += "\n";
        }
        mapping_table += "}\n";

        write_to_file(std::string_view{ mapping_table }, std::string_view{ out_filename });
    }

    std::exit(EXIT_SUCCESS);
}
