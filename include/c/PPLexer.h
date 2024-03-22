#pragma once

#include "c/Lexer.h"
#include "support/Utility.h"
#include <cassert>
#include <cstdlib>
#include <format>
#include <iostream>
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
  Lexer lex;
  std::string content;
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
  LL1LexerAdapter(BoT &backOff) : backOff(backOff) { next(); }

  Token next() {
    Token tok = currTok;
    currTok = backOff.next();
    return tok;
  }

  Token peek() { return currTok; }

  BoT &backOff;

  Token currTok;
};

template <typename BoT>
class StackLexerAdapter : public LL1Lexer<StackLexerAdapter<BoT>> {
public:
  StackLexerAdapter(BoT *backOff) : backOff(backOff) {}

  Token peek() {
    if (stack.empty()) {
      if (backOff) {
        return backOff->peek();
      } else {
        return Token(Token::END);
      }
    }
    return stack.back();
  }

  Token next() {
    if (stack.empty()) {
      if (backOff) {
        return backOff->next();
      } else {
        return Token(Token::END);
      }
    }
    Token tok = stack.back();
    stack.pop_back();
    return tok;
  }

  BoT *backOff;

  std::vector<Token> stack;
};

class PPLexer : public LL1Lexer<PPLexer> {
public:
  PPLexer(Lexer &lex) : lex(lex) { getToken(); }

  Token next() {
    Token tok = currTok;
    getToken();
    return tok;
  }
  Token peek() { return currTok; }

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
    auto it = ppKeywords.find(
        std::string_view(lex.nextSkipWhiteSpaceExceptNewLine()));
    if (it == ppKeywords.end()) {
      lex.dropLine();
      return true;
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
      case PPDirective::INCLUDE:
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
    ReplacementCtx(PPLexer &pp, LL1LexerAdapter<Lexer> *lex)
        : pp(pp), lex(lex) {}
    StackLexerAdapter<LL1LexerAdapter<Lexer>> lex;
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
          ctx.lex.stack.insert(ctx.lex.stack.end(), arg.rbegin(), arg.rend());
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
              lex.stack.insert(lex.stack.end(), args[paramIt->second].rbegin(),
                               args[paramIt->second].rend());
              continue;
            }
          }
          lex.stack.push_back(tok);
        }
      } else {
        for (auto it = s->replacementList.rbegin(),
                  itEnd = s->replacementList.rend();
             it != itEnd; ++it) {
          lex.stack.push_back(*it);
        }
      }
    }

    void emitInvalid() { lex.stack.push_back(Token::INVALID); }
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
    while (toks.back().isWhiteSpace()) {
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

  LL1LexerAdapter<Lexer> lex;
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
