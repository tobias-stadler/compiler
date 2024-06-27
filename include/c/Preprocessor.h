#pragma once

#include "c/AST.h"
#include "c/Lexer.h"
#include "c/LexerAdapter.h"
#include <cassert>
#include <cstdlib>
#include <filesystem>
#include <optional>
#include <string_view>
#include <vector>

namespace c {

class PPSymbol {
public:
  bool addParam(std::string_view paramName);

public:
  std::string_view name;
  std::vector<Token> replacementList;

  bool functionLike = false;
  std::vector<std::string_view> params;
  std::unordered_map<std::string_view, std::size_t> paramIndex;
};

class PPSymbolTable {
public:
  PPSymbol *defineSymbol(PPSymbol &&sym);
  bool undefSymbol(std::string_view name);
  PPSymbol *getSymbol(std::string_view name);

private:
  std::unordered_map<std::string_view, PPSymbol> symbols;
};

class PPTranslationUnit {
public:
  struct LineRange {
    size_t begin;
    size_t end;
  };

  static std::optional<PPTranslationUnit> load(std::filesystem::path filePath);

  std::optional<std::string_view> getLine(size_t n);
  std::optional<LineRange> getLineRange(std::string_view str);

public:
  std::filesystem::path path;
  std::string content;
  std::vector<const char *> newlines;
  bool done = false;
};

enum class PPDirective {
  IF_START,
  IF,
  IFDEF,
  IFNDEF,
  ELIF,
  ELSE,
  ENDIF,
  IF_END,
  INCLUDE,
  DEFINE,
  UNDEF,
  LINE,
  ERROR,
  PRAGMA,
};

constexpr bool isIfDirective(PPDirective directive) {
  return directive > PPDirective::IF_START && directive < PPDirective::IF_END;
}

class IncludeLexer {
public:
  struct Include {
    Include(PPTranslationUnit &tu) : tu(tu), lex(tu.content) {}

    PPTranslationUnit &tu;
    Lexer lex;
  };

  struct LineRange {
    PPTranslationUnit &tu;
    PPTranslationUnit::LineRange range;
  };

  IncludeLexer() {}

  Token next();
  bool includeHeaderName(std::string_view headerName, bool enableLocal);
  bool includePath(std::filesystem::path path);

  Include &currInclude() {
    assert(!includeStack.empty());
    return includeStack.back();
  }

  std::optional<LineRange> getLineRange(std::string_view str);

public:
  std::vector<Include> includeStack;
  std::vector<std::filesystem::path> basePaths;
  std::unordered_map<std::string, PPTranslationUnit> translationUnits;
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
    // If lex is set to nullptr, only the tokens in the stack are available for
    // replacement
    ReplacementCtx(PPLexer &pp, Lex *lex) : pp(pp), lex(lex) {}

    std::vector<Token> gobbleArgument();
    void replaceAll();
    void replaceOnce();
    void replaceFunctionLike(PPSymbol &sym);
    void replaceObjectLike(PPSymbol &sym);

    void emitInvalid() { lex.tokStack.push_back(Token::INVALID); }

    PPLexer &pp;
    StackLexerAdapter<Lex> lex;
    std::vector<Token> out;
  };

  struct IfPPDirective {
    bool enableGroup = false;
    bool wasEnabled = false;
    bool hadElse = false;
  };

  void getToken();
  bool isGroupEnabled();
  ASTPtrResult parseConstExpression();
  bool handlePPDirective();
  bool handleIfDirective(PPDirective directive);
  bool handleNonIfDirective(PPDirective directive);
  bool handleIncludeDirective();
  void handleAllPPDirectives();
  std::vector<Token> gobbleReplacementList();
  void probeKeyword();

  void error(std::string_view str);
  void errorExpectedToken(std::string_view str);
  void errorExpectedToken(Token::Kind expectedKind);

private:
  ASTContext &ctx;
  IncludeLexer &incLex;
  Lex lex;
  ReplacementCtx replacementCtx{*this, &lex};
  Token currTok;
  PPSymbolTable &sym;
  std::vector<IfPPDirective> ifDirectiveStack;

  static std::unordered_map<std::string_view, Token::Kind> keywords;
  static std::unordered_map<std::string_view, PPDirective> ppKeywords;
};

} // namespace c
