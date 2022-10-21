//
// Created by coder2k on 15.10.2022.
//

#include "upholsterer.hpp"
#include "error.hpp"
#include "magic_enum_wrapper.hpp"
#include "utils.hpp"
#include <cstdio>
#include <fmt/core.h>
#include <stdexcept>
#include <upholsterer2k/lexer.h>
#include <upholsterer2k/string_view.h>
#include <upholsterer2k/token.h>
#include <upholsterer2k/token_vector.h>
#include <upholsterer2k/upholsterer2k.h>

#ifdef _WIN32
extern "C" {
#include <fcntl.h>
#include <io.h>
}
#endif

using namespace Bssembler;

namespace upholsterer2k {

    [[nodiscard]] static UP2K_StringView c_string_view(const std::string_view view) {
        return UP2K_string_view_from_pointers(view.data(), view.data() + view.length());
    }

    [[nodiscard]] static std::string c_string_view_to_string(const UP2K_StringView view) {
        return std::string{ view.data, view.length };
    }

    [[nodiscard]] static std::string_view c_string_view_to_string_view(const UP2K_StringView view) {
        return std::string_view{ view.data, view.length };
    }

    [[nodiscard]] static std::optional<Bssembler::Register> token_to_register(const UP2K_Token token) {
        assert(token.type == TOKEN_TYPE_REGISTER);
        auto register_number_string = c_string_view_to_string(token.string_view).substr(1);
        try {
            const auto register_number = std::stoi(register_number_string);
            if (register_number >= 0 and register_number <= 255) {
                const auto register_string = fmt::format("R{}", register_number);
                const auto register_ = magic_enum::enum_cast<Bssembler::Register>(register_string);
                assert(register_.has_value());
                return *register_;
            } else {
                return {};
            }
        } catch (const std::out_of_range&) {
            // do nothing
        } catch (const std::invalid_argument&) {
            assert(false and "unreachable"); // Upholsterer Lexer should have checked this
        }
        return {};
    }

    [[nodiscard]] Bssembly::InstructionVector
    parse_bssembly(const std::string_view filename, std::string_view source, Location location) {
        source = Utils::trim(source);
        const auto source_string = fmt::format("{}\n", source);
        auto constants = constants_map_create(16);
        UP2K_fill_constants_map(&constants);
        const auto source_file = UP2K_SourceFile{
            .filename{ c_string_view(filename) },
            .source{ c_string_view(source_string) },
        };
        auto tokens = UP2K_tokenize(source_file, &constants);

        auto instructions = Bssembly::InstructionVector{};

        usize i = 0;
        while (true) {
            while (i < tokens.size and tokens.data[i].type == TOKEN_TYPE_NEWLINE) {
                ++i;
            }
            if (i >= tokens.size or tokens.data[i].type == TOKEN_TYPE_EOF) {
                break;
            }
            if (tokens.data[i].type == TOKEN_TYPE_DOT) { // literal
                ++i;
                const auto is_identifier = tokens.data[i].type == TOKEN_TYPE_IDENTIFIER;
                const auto uppercase_identifier = Utils::to_upper(c_string_view_to_string(tokens.data[i].string_view));
                if (is_identifier and uppercase_identifier == "STRING") { // string literal
                    ++i;
                    if (tokens.data[i].type != TOKEN_TYPE_STRING_LITERAL) {
                        Error::error(
                                Location{
                                        .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                        .offset_start_inclusive{ 0 },
                                        .offset_end_exclusive{ 1 },
                                },
                                "expected string literal"
                        );
                    }
                    instructions.emplace_back(StringLiteral{ c_string_view_to_string(tokens.data[i].string_view) });
                } else if (is_identifier and uppercase_identifier == "WORDS") { // words literal
                    ++i;
                    if (tokens.data[i].type != TOKEN_TYPE_LEFT_BRACKET) {
                        Error::error(
                                Location{
                                        .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                        .offset_start_inclusive{ 0 },
                                        .offset_end_exclusive{ 1 },
                                },
                                "expected \"[\""
                        );
                    }
                    assert(false and "not implemented");
                } else {
                    Error::error(
                            Location{
                                    .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                    .offset_start_inclusive{ 0 },
                                    .offset_end_exclusive{ 1 },
                            },
                            "expected \"string\" or \"words\""
                    );
                }
                ++i;
                continue;
            }
            if (tokens.data[i].type == TOKEN_TYPE_IDENTIFIER and tokens.data[i + 1].type == TOKEN_TYPE_COLON) { // label
                instructions.push_back(Label{ c_string_view_to_string(tokens.data[i].string_view) });
                i += 2;
                if (tokens.data[i].type != TOKEN_TYPE_NEWLINE) {
                    Error::error(
                            Location{
                                    .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                    .offset_start_inclusive{ 0 },
                                    .offset_end_exclusive{ 1 },
                            },
                            "expected newline"
                    );
                }
                ++i;
                continue;
            }
            if (tokens.data[i].type != TOKEN_TYPE_IDENTIFIER) {
                Error::error(
                        Location{
                                .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                .offset_start_inclusive{ 0 },
                                .offset_end_exclusive{ 1 },
                        },
                        fmt::format("expected mnemonic, got \"{}\"", magic_enum::enum_name(tokens.data[i].type))
                );
            }

            // all mnemonics in the Mnemonic enum class are upper case, so we have to convert the mnemonic to upper case
            // as well
            auto mnemonic_string = Utils::to_upper(std::string_view{ tokens.data[i].string_view.data,
                                                                     tokens.data[i].string_view.length });

            const auto mnemonic_optional = magic_enum::enum_cast<Mnemonic>(mnemonic_string);
            if (not mnemonic_optional.has_value()) {
                Error::error(
                        Location{
                                .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                .offset_start_inclusive{ 0 },
                                .offset_end_exclusive{ 1 },
                        },
                        fmt::format(
                                "unknown mnemonic \"{}\"",
                                std::string_view{ tokens.data[i].string_view.data, tokens.data[i].string_view.length }
                        )
                );
            }

            [[maybe_unused]] const auto mnemonic = *mnemonic_optional;
            auto arguments = std::vector<InstructionArgument>{};
            ++i;
            auto needs_more_arguments = false;
            while (needs_more_arguments
                   or (i < tokens.size and tokens.data[i].type != TOKEN_TYPE_NEWLINE
                       and tokens.data[i].type != TOKEN_TYPE_EOF)) {
                switch (tokens.data[i].type) {
                    case TOKEN_TYPE_REGISTER: {
                        const auto register_ = token_to_register(tokens.data[i]);
                        if (register_.has_value()) {
                            arguments.emplace_back(*register_);
                        } else {
                            Error::error(
                                    Location{
                                            .source_code{
                                                    ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                            .offset_start_inclusive{ 0 },
                                            .offset_end_exclusive{ 1 },
                                    },
                                    fmt::format(
                                            "register number \"{}\" out of range",
                                            c_string_view_to_string_view(tokens.data[i].string_view)
                                    )
                            );
                        }
                        break;
                    }
                    case TOKEN_TYPE_REGISTER_CONSTANT: { // (e.g. "SP")
                        const auto register_constant_string =
                                Utils::to_upper(c_string_view_to_string_view(tokens.data[i].string_view));
                        const auto register_ = magic_enum::enum_cast<Bssembler::Register>(register_constant_string);
                        assert(register_.has_value() and "Upholsterer lexer should have caught this");
                        arguments.emplace_back(*register_);
                        break;
                    }
                    case TOKEN_TYPE_WORD_LITERAL: {
                        bool success;
                        UP2K_Word result;
                        UP2K_word_from_token(&tokens.data[i], &success, &result);
                        assert(success and "Upholsterer lexer should have caught this");
                        arguments.emplace_back(Immediate{ static_cast<usize>(result) });
                        break;
                    }
                    case TOKEN_TYPE_WORD_CONSTANT: {
                        auto word_constant_string =
                                Utils::to_upper(c_string_view_to_string_view(tokens.data[i].string_view));
                        arguments.emplace_back(WordConstant{ std::move(word_constant_string) });
                        break;
                    }
                    case TOKEN_TYPE_IDENTIFIER: { // constants like TERMINAL_END
                        // this isn't really a label, but we can treat it like it was, because it behaves the same
                        arguments.emplace_back(LabelArgument{ c_string_view_to_string(tokens.data[i].string_view) });
                        break;
                    }
                    case TOKEN_TYPE_ASTERISK: {
                        ++i;
                        switch (tokens.data[i].type) {
                            case TOKEN_TYPE_REGISTER: {
                                const auto register_ = token_to_register(tokens.data[i]);
                                if (register_.has_value()) {
                                    arguments.emplace_back(Pointer{ *register_ });
                                } else {
                                    Error::error(
                                            Location{
                                                    .source_code{ ::SourceCode{ .filename{ filename },
                                                                                .text{ source_string } } },
                                                    .offset_start_inclusive{ 0 },
                                                    .offset_end_exclusive{ 1 },
                                            },
                                            fmt::format(
                                                    "register number \"{}\" out of range",
                                                    c_string_view_to_string_view(tokens.data[i].string_view)
                                            )
                                    );
                                }
                                break;
                            }
                            case TOKEN_TYPE_REGISTER_CONSTANT: {
                                const auto register_constant_string =
                                        Utils::to_upper(c_string_view_to_string_view(tokens.data[i].string_view));
                                const auto register_ =
                                        magic_enum::enum_cast<Bssembler::Register>(register_constant_string);
                                assert(register_.has_value() and "Upholsterer lexer should have caught this");
                                arguments.emplace_back(Pointer{ *register_ });
                                break;
                            }
                            case TOKEN_TYPE_WORD_CONSTANT: {
                                arguments.emplace_back(Pointer{
                                        WordConstant{ c_string_view_to_string(tokens.data[i].string_view) } });
                                break;
                            }
                            case TOKEN_TYPE_IDENTIFIER: { // label
                                arguments.emplace_back(Pointer{
                                        LabelArgument{ c_string_view_to_string(tokens.data[i].string_view) } });
                                break;
                            }
                            default:
                                assert(false and "unreachable");
                                break;
                        }

                        break;
                    }
                    default:
                        Error::error(
                                Location{
                                        .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                        .offset_start_inclusive{ 0 },
                                        .offset_end_exclusive{ 1 },
                                },
                                fmt::format(
                                        "unexpected token type \"{}\" (\"{}\")",
                                        magic_enum::enum_name(tokens.data[i].type),
                                        c_string_view_to_string_view(tokens.data[i].string_view)
                                )
                        );
                        break;
                }
                ++i;
                if (tokens.data[i].type == TOKEN_TYPE_COMMA) {
                    ++i;
                    needs_more_arguments = true;
                } else if (tokens.data[i].type != TOKEN_TYPE_NEWLINE and tokens.data[i].type != TOKEN_TYPE_EOF) {
                    Error::error(
                            Location{
                                    .source_code{ ::SourceCode{ .filename{ filename }, .text{ source_string } } },
                                    .offset_start_inclusive{ 0 },
                                    .offset_end_exclusive{ 1 },
                            },
                            "argument expected"
                    );
                } else {
                    needs_more_arguments = false;
                }
            }
            instructions.push_back(Bssembler::Instruction{ mnemonic, std::move(arguments), location });
            ++i;
        }

        UP2K_token_vector_free(&tokens);
        constants_map_free(&constants);

        return instructions;
    }

    [[nodiscard]] bool
    write_machine_code_to_file(const UP2K_ByteVector machine_code, const std::optional<std::filesystem::path>& path) {
        if (path.has_value()) {
#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable: 4996 )
#endif
            const auto file = std::fopen(path->string().c_str(), "wb");
#ifdef _MSC_VER
#pragma warning( pop )
#endif
            if (file == nullptr) {
                return false;
            }
            UP2K_write_machine_code(machine_code, file);

            std::fclose(file);
        } else {
            // output to stdout

            // when in windows, we have to set the mode of stdout to binary because otherwise
            // every \n will be automatically replaced with \r\n which destroys the generated
            // binary
#ifdef _WIN32
            _setmode(_fileno(stdout), _O_BINARY);
#endif
            UP2K_write_machine_code(machine_code, stdout);
        }
        return true;
    }

} // namespace upholsterer2k
