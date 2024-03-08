#include "c/PPLexer.h"
#include <iostream>

#define PPLEXER_DEBUG

namespace c {

bool PPLexer::matchPeek(Token::Kind kind) {
  if (currTok.kind != kind)
    return false;

  return true;
}

Token PPLexer::next() {
  Token tok = currTok;
  getToken();
  return tok;
}

Token PPLexer::peek() { return currTok; }

void PPLexer::drop() { getToken(); }

bool PPLexer::matchNext(Token::Kind kind) {
  if (currTok.kind != kind)
    return false;

  getToken();
  return true;
}

Token::Kind PPLexer::peekKind() { return currTok.kind; };

void PPLexer::getToken() {
#ifdef PPLEXER_DEBUG
  std::cerr << "[Debug][PPLexer]";
#endif
  assert(!currTok.isEnd());
  do {
    currTok = lex.next();
#ifdef PPLEXER_DEBUG
    std::cerr << " " << currTok;
#endif
  } while (currTok.isWhiteSpace());
#ifdef PPLEXER_DEBUG
  std::cerr << "\n";
#endif
}
} // namespace c
