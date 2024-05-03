#pragma once

#include "c/AST.h"
#include "c/Lexer.h"
#include "support/Utility.h"
#include <cassert>
#include <cstdlib>
#include <filesystem>
#include <iterator>
#include <optional>
#include <string_view>
#include <vector>

namespace c {

class PPSymbol {
public:
  std::string_view name;
  std::vector<Token> replacementList;

  bool functionLike = false;
  std::vector<std::string_view> params;
  std::unordered_map<std::string_view, std::size_t> paramIndex;

  bool addParam(std::string_view paramName);
};

class PPSymbolTable {
public:
  std::unordered_map<std::string_view, PPSymbol> symbols;

  PPSymbol *defineSymbol(PPSymbol &&sym);

  bool undefSymbol(std::string_view name);

  PPSymbol *getSymbol(std::string_view name);
};

class PPTranslationUnit {
public:
  std::filesystem::path path;
  std::string content;
  std::vector<const char *> newlines;
  bool done = false;

  static std::optional<PPTranslationUnit> load(std::filesystem::path filePath);

  struct LineRange {
    size_t begin;
    size_t end;
  };

  std::optional<std::string_view> getLine(size_t n);

  std::optional<LineRange> getLineRange(std::string_view str);
};

enum class PPDirective {
  IF,
  IFDEF,
  IFNDEF,
  ELIF,
  ELSE,
  ENDIF,
  INCLUDE,
  DEFINE,
  UNDEF,
  LINE,
  ERROR,
  PRAGMA,
};

template <typename C> class LL1Lexer {
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

private:
  C &impl() { return static_cast<C &>(*this); }
};

template <typename BoT>
class LL1LexerAdapter : public LL1Lexer<LL1LexerAdapter<BoT>> {
public:
  LL1LexerAdapter(BoT &backOff) : backOff(backOff) {}

  Token next() {
    Token tok = currTok;
    currTok = backOff.next();
    return tok;
  }

  Token peek() { return currTok; }

  BoT &backOff;

  Token currTok;
};

class IncludeLexer {
public:
  IncludeLexer() {}

  Token next();

  bool includeHeaderName(std::string_view headerName, bool enableLocal);

  bool includePath(std::filesystem::path path);

  struct Include {
    Include(PPTranslationUnit &tu) : tu(tu), lex(tu.content) {}

    PPTranslationUnit &tu;
    Lexer lex;
  };

  Include &currInclude() {
    assert(!includeStack.empty());
    return includeStack.back();
  }

  struct LineRange {
    PPTranslationUnit &tu;
    PPTranslationUnit::LineRange range;
  };

  std::optional<LineRange> getLineRange(std::string_view str);

  std::vector<Include> includeStack;

  std::vector<std::filesystem::path> basePaths;

  std::unordered_map<std::string, PPTranslationUnit> translationUnits;
};

template <typename BoT>
class StackLexerAdapter : public LL1Lexer<StackLexerAdapter<BoT>> {
public:
  StackLexerAdapter(BoT *backOff) : backOff(backOff) {}

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

  BoT *backOff;

  std::vector<Token> tokStack;
};

class PPLexer : public LL1Lexer<PPLexer> {
public:
  using Lex = LL1LexerAdapter<IncludeLexer>;

  PPLexer(ASTContext &ctx, IncludeLexer &incLex, PPSymbolTable &sym);

  Token next() {
    Token tok = currTok;
    getToken();
    return tok;
  }

  Token peek() { return currTok; }

  void printErrCtx(std::ostream &os, std::string_view str);

private:
  struct ReplacementCtx {
    PPLexer &pp;
    ReplacementCtx(PPLexer &pp, Lex *lex) : pp(pp), lex(lex) {}
    StackLexerAdapter<Lex> lex;
    std::vector<Token> out;

    std::vector<Token> gobbleArgument();

    void replaceAll();

    void replaceOnce();

    void emitInvalid() { lex.tokStack.push_back(Token::INVALID); }
  };

  struct PPIf {
    bool enableGroup = false;
    bool wasEnabled = false;
    bool hadElse = false;
  };

  void getToken();

  bool isGroupEnabled();

  ASTPtrResult parseConstExpression();

  bool handlePPDirective();

  void errorExpectedToken(std::string_view str);

  void error(std::string_view str);

  void errorExpectedToken(Token::Kind expectedKind);

  void handleAllPPDirectives();

  std::vector<Token> gobbleReplacementList();

  void probeKeyword();

  ASTContext &ctx;
  IncludeLexer &incLex;
  Lex lex;
  ReplacementCtx replacementCtx{*this, &lex};
  Token currTok;
  PPSymbolTable &sym;
  std::vector<PPIf> ifStack;

  static std::unordered_map<std::string_view, Token::Kind> keywords;
  static std::unordered_map<std::string_view, PPDirective> ppKeywords;
};
} // namespace c
