#pragma once

#include <cstdint>
#include <string_view>

class Token {
public:
  enum Kind {
    TOK_SEMICOLON,
    TOK_STAR,
    TOK_SINGLEQUOTE,
    TOK_DOUBLEQUOTE,
    TOK_IDENTIFIER,
    TOK_LITERAL_NUM,
    TOK_LITERAL_STRING
  };

  Token(Kind kind, std::string_view text);
  const Kind kind;
  const std::string_view text;
};

class Lexer {
public:
  Lexer(std::string_view input);
  Token nextToken();

private:
  const std::string_view input;
  std::string_view::iterator currPosition;
};
