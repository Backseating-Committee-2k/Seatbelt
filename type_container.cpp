//
// Created by coder2k on 13.07.2022.
//

#include "type_container.hpp"
#include "data_type.hpp"
#include "error.hpp"
#include <algorithm>
#include <ranges>

[[nodiscard]] static bool are_types_equal(const std::unique_ptr<DataType>& lhs, const std::unique_ptr<DataType>& rhs) {
    return static_cast<bool>(lhs) and static_cast<bool>(rhs) and *lhs == *rhs;
}

[[nodiscard]] static std::unique_ptr<DataType> token_to_type(const Lexer::Tokens::Token token, const bool is_mutable) {
    return std::make_unique<ConcreteType>(Error::token_location(token).view(), is_mutable);
}

[[nodiscard]] static std::unique_ptr<DataType>
tokens_to_type(const std::span<const Lexer::Tokens::Token> tokens, const bool is_mutable) {
    auto result = token_to_type(tokens.back(), is_mutable);
    for (auto iterator = std::crbegin(tokens) + 1; iterator != std::crend(tokens); ++iterator) {
        if (not std::holds_alternative<Lexer::Tokens::Arrow>(*iterator)) {
            Error::error(*iterator, "invalid type specifier");
        }
        result = std::make_unique<PointerType>(std::move(result), false);
    }
    return result;
}

[[nodiscard]] const DataType*
TypeContainer::from_tokens(const std::span<const Lexer::Tokens::Token> type_tokens, const bool is_mutable) {
    auto data_type = tokens_to_type(type_tokens, is_mutable);
    return from_data_type(std::move(data_type));
}

[[nodiscard]] const DataType* TypeContainer::from_data_type(std::unique_ptr<DataType> data_type) {
    using std::ranges::find_if;
    const auto find_iterator =
            find_if(m_data_types, [&](const auto& other) { return are_types_equal(data_type, other); });
    const auto data_type_found = find_iterator != std::cend(m_data_types);
    if (data_type_found) {
        return find_iterator->get();
    } else {
        m_data_types.push_back(std::move(data_type));
        return m_data_types.back().get();
    }
}
