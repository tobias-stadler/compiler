#pragma once

#include "c/Lexer.h"
#include <vector>

namespace c {

template <typename DerivedT> class LL1Lexer {
public:
  void drop() { impl().next(); }

  bool matchPeek(Token::Kind kind) {
    if (peekKind() != kind)
      return false;
    return true;
  }

  bool matchNext(Token::Kind kind) {
    if (peekKind() != kind)
      return false;
    drop();
    return true;
  }

  Token::Kind peekKind() { return impl().peek().kind; };

  Token nextSkipWhiteSpace() {
    Token tok = impl().next();
    skipWhiteSpace();
    return tok;
  }

  void skipWhiteSpace() {
    while (impl().peek().isWhiteSpace()) {
      drop();
    }
  }

  Token nextSkipWhiteSpaceExceptNewLine() {
    Token tok = impl().next();
    skipWhiteSpaceExceptNewLine();
    return tok;
  }

  void skipWhiteSpaceExceptNewLine() {
    while (impl().peek().isWhiteSpaceExceptNewLine()) {
      drop();
    }
  }

  void dropLine() {
    while (!impl().peek().isNewLine() && !impl().peek().isEnd()) {
      drop();
    }
    if (impl().peek().isNewLine()) {
      drop();
    }
  }

  bool hasNext() { return !matchPeek(Token::END) && !matchPeek(Token::INVALID); }

private:
  DerivedT &impl() { return static_cast<DerivedT &>(*this); }
};

template <typename BackOffT>
class LL1LexerAdapter : public LL1Lexer<LL1LexerAdapter<BackOffT>> {
public:
  LL1LexerAdapter(BackOffT &backOff) : backOff(backOff) {}

  Token next() {
    Token tok = currTok;
    currTok = backOff.next();
    return tok;
  }

  Token peek() { return currTok; }

public:
  BackOffT &backOff;
  Token currTok;
};

template <typename BackOffT>
class StackLexerAdapter : public LL1Lexer<StackLexerAdapter<BackOffT>> {
public:
  StackLexerAdapter(BackOffT *backOff) : backOff(backOff) {}

  Token peek() {
    if (tokStack.empty()) {
      if (backOff) {
        return backOff->peek();
      } else {
        return Token(Token::END);
      }
    }
    return tokStack.back();
  }

  Token next() {
    if (tokStack.empty()) {
      if (backOff) {
        return backOff->next();
      } else {
        return Token(Token::END);
      }
    }
    Token tok = tokStack.back();
    tokStack.pop_back();
    return tok;
  }

public:
  BackOffT *backOff;
  std::vector<Token> tokStack;
};

} // namespace c
