#pragma once

#include "c/Lexer.h"
#include <cassert>
#include <vector>

namespace c {

class PPSymbol {
public:
  std::string_view name;
  bool functionLike = false;
  bool disabled = false;
  std::vector<Token> replacementList;
};

class PPSymbolTable {

};

class PPLexer {
public:
  PPLexer(Lexer &lex) : lex(lex) { getToken(); }

  Token next();
  Token peek();
  void drop();
  Token::Kind peekKind();
  bool matchNext(Token::Kind kind);
  bool matchPeek(Token::Kind kind);

private:
  void getToken();

  Lexer &lex;
  Token currTok;
};
} // namespace c
