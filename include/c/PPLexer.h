#pragma once

#include "c/Lexer.h"
#include "support/Utility.h"
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
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

  bool addParam(std::string_view paramName) {
    auto [it, succ] = paramIndex.try_emplace(paramName, params.size());
    if (!succ)
      return false;
    params.push_back(paramName);
    return true;
  }
};

class PPSymbolTable {
public:
  std::unordered_map<std::string_view, PPSymbol> symbols;

  PPSymbol *defineSymbol(PPSymbol &&sym) {
    auto [it, succ] = symbols.try_emplace(sym.name, std::move(sym));
    if (!succ)
      return nullptr;

    return &it->second;
  }

  bool undefSymbol(std::string_view name) { return symbols.erase(name); }

  PPSymbol *getSymbol(std::string_view name) {
    auto it = symbols.find(name);
    if (it == symbols.end())
      return nullptr;
    return &it->second;
  }
};

class PPTranslationUnit {
public:
  std::filesystem::path path;
  std::string content;
  std::vector<const char *> newlines;
  bool done = false;

  static std::optional<PPTranslationUnit> load(std::filesystem::path filePath) {
    PPTranslationUnit res;
    res.path = filePath;
    std::ifstream f(filePath);
    if (!f.good()) {
      return std::nullopt;
    }
    auto sz = std::filesystem::file_size(filePath);
    res.content = std::string(sz, 0);
    f.read(res.content.data(), sz);
    return res;
  }

  struct LineRange {
    size_t begin;
    size_t end;
  };

  std::optional<std::string_view> getLine(size_t n) {
    if (n >= newlines.size()) {
      return std::nullopt;
    }
    const char *end = newlines[n] - 1;
    const char *begin = n > 0 ? newlines[n - 1] + 1 : content.data();
    return std::string_view(begin, end);
  }

  std::optional<LineRange> getLineRange(std::string_view str) {
    auto itBegin =
        std::lower_bound(newlines.begin(), newlines.end(), str.data());
    auto itEnd = std::lower_bound(newlines.begin(), newlines.end(),
                                  str.data() + str.size());
    if (itBegin == newlines.end()) {
      return std::nullopt;
    }
    LineRange r;
    r.begin = std::distance(newlines.begin(), itBegin);
    if (itEnd == newlines.end()) {
      r.end = newlines.size();
    } else {
      r.end = std::distance(newlines.begin(), itEnd);
    }
    return r;
  }
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

  Token next() {
    if (includeStack.empty()) {
      return Token(Token::END);
    }
    Include &inc = includeStack.back();
    Token tok = inc.lex.next();
    if (tok.isEnd()) {
      inc.tu.done = true;
      includeStack.pop_back();
      return next();
    }
    if (tok.isNewLine() && !inc.tu.done) {
      inc.tu.newlines.push_back(tok.text);
    }
    return tok;
  }

  bool includeHeaderName(std::string_view headerName, bool enableLocal) {
    if (enableLocal) {
      if (includePath(currInclude().tu.path.parent_path() / headerName)) {
        return true;
      }
    }
    for (auto &basePath : basePaths) {
      if (includePath(basePath / headerName)) {
        return true;
      }
    }
    return false;
  }

  bool includePath(std::filesystem::path path) {
    auto canPath = std::filesystem::weakly_canonical(path);
    std::cerr << "Trying to include: " << path << " (" << canPath << ")\n";
    auto it = translationUnits.find(canPath);
    if (it != translationUnits.end()) {
      includeStack.emplace_back(it->second);
      return true;
    }
    auto newTu = PPTranslationUnit::load(canPath);
    if (!newTu) {
      return false;
    }
    auto [itNew, _] =
        translationUnits.try_emplace(newTu->path, std::move(newTu.value()));
    includeStack.emplace_back(itNew->second);
    return true;
  }

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

  std::optional<LineRange> getLineRange(std::string_view str) {
    for (auto &[_, tu] : translationUnits) {
      auto r = tu.getLineRange(str);
      if (r) {
        return LineRange{tu, r.value()};
      }
    }
    return std::nullopt;
  }

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
  PPLexer(IncludeLexer &incLex) : incLex(incLex), lex(incLex) {
    lex.next();
    getToken();
  }

  Token next() {
    Token tok = currTok;
    getToken();
    return tok;
  }
  Token peek() { return currTok; }

  void printErrCtx(std::ostream &os, std::string_view str) {
    os << "Source context: ";
    auto r = incLex.getLineRange(str);
    if (!r) {
      os << "(unavailable)\n";
      return;
    }
    os << r.value().tu.path << ":" << r.value().range.begin << "-"
       << r.value().range.end;
  }

private:
  void getToken();

  bool isGroupEnabled() {
    return ifStack.empty() || ifStack.back().enableGroup;
  }

  bool handlePPDirective() {
    lex.skipWhiteSpace();
    if (!lex.matchNext(Token::PUNCT_HASH)) {
      if (!isGroupEnabled()) {
        lex.dropLine();
        return true;
      }
      return false;
    }
    lex.skipWhiteSpaceExceptNewLine();
    if (!lex.matchPeek(Token::IDENTIFIER)) {
      lex.dropLine();
      return true;
    }
    auto it = ppKeywords.find(std::string_view(lex.peek()));
    if (it == ppKeywords.end()) {
      lex.dropLine();
      return true;
    }
    if (it->second != PPDirective::INCLUDE) {
      lex.nextSkipWhiteSpaceExceptNewLine();
    }
    switch (it->second) {
    case PPDirective::IFNDEF:
    case PPDirective::IFDEF: {
      if (!lex.matchPeek(Token::IDENTIFIER)) {
        errorExpectedToken(Token::IDENTIFIER);
        return false;
      }
      if (isGroupEnabled()) {
        auto *s = sym.getSymbol(lex.peek());
        bool isTrue = bool(s) ^ (it->second == PPDirective::IFNDEF);
        ifStack.push_back({isTrue, isTrue});
      } else {
        ifStack.push_back({false});
      }
      lex.drop();
      break;
    }
    case PPDirective::ELSE: {
      if (ifStack.empty()) {
        error("#else without prior if");
        return false;
      }
      if (ifStack.back().hadElse) {
        error("#else after prior #else");
        return false;
      }
      auto g = ifStack.back();
      ifStack.pop_back();
      if (isGroupEnabled()) {
        ifStack.push_back({!g.wasEnabled, true, true});
      } else {
        ifStack.push_back({false, false, true});
      }
      break;
    }
    case PPDirective::ELIF: {
      if (ifStack.empty()) {
        error("#elif without prior if");
        return false;
      }
      // TODO: implement
      auto g = ifStack.back();
      ifStack.pop_back();
      if (isGroupEnabled()) {
        bool isTrue = !g.wasEnabled && false;
        ifStack.push_back({isTrue, g.wasEnabled || isTrue, false});
      } else {
        ifStack.push_back({false, false, false});
      }
      break;
    }
    case PPDirective::ENDIF: {
      if (ifStack.empty()) {
        error("#endif without prior if");
        return false;
      }
      ifStack.pop_back();
      break;
    }
    case PPDirective::IF:
      UNREACHABLE("#if unimplemented");
      break;
    case PPDirective::INCLUDE:
    case PPDirective::DEFINE:
    case PPDirective::UNDEF:
    case PPDirective::LINE:
    case PPDirective::ERROR:
    case PPDirective::PRAGMA:
      break;
    }
    if (isGroupEnabled()) {
      switch (it->second) {
      case PPDirective::DEFINE: {
        if (!lex.matchPeek(Token::IDENTIFIER)) {
          errorExpectedToken(Token::IDENTIFIER);
          return false;
        }
        PPSymbol s;
        s.name = lex.nextSkipWhiteSpaceExceptNewLine();
        if (lex.matchPeek(Token::PUNCT_PARENO)) {
          s.functionLike = true;
          lex.drop();
          do {
            lex.skipWhiteSpaceExceptNewLine();
            if (lex.matchPeek(Token::PUNCT_PARENC)) {
              break;
            }
            if (lex.matchNext(Token::PUNCT_DOTDOTDOT)) {
              break;
            }
            if (!lex.matchPeek(Token::IDENTIFIER)) {
              errorExpectedToken(Token::IDENTIFIER);
              return false;
            }
            s.addParam(lex.nextSkipWhiteSpaceExceptNewLine());
          } while (lex.matchNext(Token::PUNCT_COMMA));
          lex.skipWhiteSpaceExceptNewLine();
          if (!lex.matchNext(Token::PUNCT_PARENC)) {
            errorExpectedToken(Token::PUNCT_PARENC);
            return false;
          }
        }
        s.replacementList = gobbleReplacementList();
        auto *sPtr = sym.defineSymbol(std::move(s));
        if (!sPtr) {
          error("Redefined macro name");
          return false;
        }
        break;
      }
      case PPDirective::UNDEF: {
        if (!lex.matchPeek(Token::IDENTIFIER)) {
          errorExpectedToken(Token::IDENTIFIER);
          return false;
        }
        sym.undefSymbol(lex.next());
        break;
      }
      case PPDirective::IF:
      case PPDirective::IFDEF:
      case PPDirective::IFNDEF:
      case PPDirective::ELIF:
      case PPDirective::ELSE:
      case PPDirective::ENDIF:
        break;
      case PPDirective::INCLUDE: {
        incLex.currInclude().lex.setEnableHeaderNames(true);
        lex.nextSkipWhiteSpaceExceptNewLine();
        if (!lex.matchPeek(Token::HEADER_NAME)) {
          errorExpectedToken(Token::HEADER_NAME);
          return false;
        }
        incLex.currInclude().lex.setEnableHeaderNames(false);
        Token tok = lex.nextSkipWhiteSpaceExceptNewLine();
        if (!lex.matchPeek(Token::NEWLINE)) {
          errorExpectedToken(Token::NEWLINE);
          return false;
        }
        std::string_view hdrName = tok;
        hdrName.remove_prefix(1);
        hdrName.remove_suffix(1);
        if (!incLex.includeHeaderName(hdrName, tok.text[0] == '"')) {
          error("Couldn't open file");
          return false;
        }
        lex.drop();
        return true;
      }
      case PPDirective::LINE:
      case PPDirective::ERROR:
      case PPDirective::PRAGMA:
        UNREACHABLE("Unimplemented preprocessing directive");
      }
    }
    lex.skipWhiteSpaceExceptNewLine();
    if (!lex.matchNext(Token::NEWLINE)) {
      errorExpectedToken(Token::NEWLINE);
      return false;
    }
    return true;
  }

  struct ReplacementCtx {
    PPLexer &pp;
    ReplacementCtx(PPLexer &pp, Lex *lex) : pp(pp), lex(lex) {}
    StackLexerAdapter<Lex> lex;
    std::vector<Token> out;

    std::vector<Token> gobbleArgument() {
      std::vector<Token> res;
      size_t open = 0;
      while (true) {
        Token tok = lex.peek();
        switch (tok.kind) {
        case Token::END:
          return res;
        case Token::PUNCT_PARENO:
          ++open;
          res.push_back(lex.next());
          break;
        case Token::PUNCT_PARENC:
          if (!open)
            return res;
          --open;
          res.push_back(lex.next());
          break;
        case Token::PUNCT_COMMA:
          if (!open)
            return res;
          res.push_back(lex.next());
          break;
        default:
          res.push_back(lex.next());
          break;
        }
      }
    }

    void handleAll() {
      while (!lex.matchPeek(Token::END) && !lex.matchPeek(Token::INVALID)) {
        handleReplacement();
      }
    }

    void handleReplacement() {
      Token tok = lex.next();
      if (tok.kind != Token::IDENTIFIER) {
        out.push_back(tok);
        return;
      }
      auto *s = pp.sym.getSymbol(std::string_view(tok));
      if (!s) {
        out.push_back(tok);
        return;
      }
      if (s->functionLike) {
        auto sz = out.size();
        out.push_back(tok);
        while (lex.peek().isWhiteSpace()) {
          out.push_back(lex.next());
        }
        if (!lex.matchNext(Token::PUNCT_PARENO)) {
          return;
        }
        out.resize(sz);
        std::vector<std::vector<Token>> args;
        do {
          lex.skipWhiteSpace();
          args.push_back(gobbleArgument());
          lex.skipWhiteSpace();
          trimWhiteSpaceBack(args.back());
        } while (lex.matchNext(Token::PUNCT_COMMA));
        if (!lex.matchNext(Token::PUNCT_PARENC)) {
          pp.errorExpectedToken(Token::PUNCT_PARENC);
          return;
        }
        for (auto &arg : args) {
          ReplacementCtx ctx{pp, nullptr};
          ctx.lex.tokStack.insert(ctx.lex.tokStack.end(), arg.rbegin(),
                                  arg.rend());
          ctx.handleAll();
          arg = std::move(ctx.out);
        }
        assert(args.size() == s->params.size());
        for (auto it = s->replacementList.rbegin(),
                  itEnd = s->replacementList.rend();
             it != itEnd; ++it) {
          Token tok = *it;
          if (tok.kind == Token::IDENTIFIER) {
            auto paramIt = s->paramIndex.find(tok);
            if (paramIt != s->paramIndex.end()) {
              lex.tokStack.insert(lex.tokStack.end(),
                                  args[paramIt->second].rbegin(),
                                  args[paramIt->second].rend());
              continue;
            }
          }
          lex.tokStack.push_back(tok);
        }
      } else {
        for (auto it = s->replacementList.rbegin(),
                  itEnd = s->replacementList.rend();
             it != itEnd; ++it) {
          lex.tokStack.push_back(*it);
        }
      }
    }

    void emitInvalid() { lex.tokStack.push_back(Token::INVALID); }
  };

  void errorExpectedToken(std::string_view str) {
    std::cerr << "[Error][Preprocessing] Expected " << str << ", but got "
              << lex.peek() << '\n';
    ctx.emitInvalid();
  }

  void error(std::string_view str) {
    std::cerr << "[Error][Preprocessing] " << str << '\n';
    ctx.emitInvalid();
  }

  void errorExpectedToken(Token::Kind expectedKind) {
    errorExpectedToken(std::format("'{}'", Token::kindName(expectedKind)));
  }

  void handleAllPPDirectives() {
    while (handlePPDirective()) {
    }
  }

  std::vector<Token> gobbleReplacementList() {
    std::vector<Token> res;
    lex.skipWhiteSpaceExceptNewLine();
    while (!lex.peek().isNewLine() && !lex.peek().isEnd()) {
      res.push_back(lex.next());
    }
    trimWhiteSpaceBack(res);
    return res;
  }

  static void trimWhiteSpaceBack(std::vector<Token> &toks) {
    while (!toks.empty() && toks.back().isWhiteSpace()) {
      toks.pop_back();
    }
  }

  void probeKeyword() {
    if (currTok.kind != Token::IDENTIFIER) {
      return;
    }
    auto it = keywords.find(std::string_view(currTok));
    if (it == keywords.end())
      return;
    currTok.kind = it->second;
  }

  IncludeLexer &incLex;
  Lex lex;
  ReplacementCtx ctx{*this, &lex};
  Token currTok;
  PPSymbolTable sym;

  struct PPIf {
    bool enableGroup = false;
    bool wasEnabled = false;
    bool hadElse = false;
  };
  std::vector<PPIf> ifStack;

  std::unordered_map<std::string_view, std::string> contents;

  static std::unordered_map<std::string_view, Token::Kind> keywords;
  static std::unordered_map<std::string_view, PPDirective> ppKeywords;
};
} // namespace c
