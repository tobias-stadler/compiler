#include "c/Preprocessor.h"
#include "c/ASTPrinter.h"
#include "c/Parser.h"
#include "c/Symbol.h"
#include <algorithm>
#include <cassert>
#include <format>
#include <fstream>
#include <iostream>

#define PPLEXER_DEBUG

namespace c {

namespace {

void trimWhiteSpaceBack(std::vector<Token> &toks) {
  while (!toks.empty() && toks.back().isWhiteSpace()) {
    toks.pop_back();
  }
}

} // namespace

std::unordered_map<std::string_view, Token::Kind> PPLexer::keywords = {
    {"while", Token::KEYWORD_WHILE},
    {"volatile", Token::KEYWORD_VOLATILE},
    {"void", Token::KEYWORD_VOID},
    {"unsigned", Token::KEYWORD_UNSIGNED},
    {"union", Token::KEYWORD_UNION},
    {"typedef", Token::KEYWORD_TYPEDEF},
    {"switch", Token::KEYWORD_SWITCH},
    {"struct", Token::KEYWORD_STRUCT},
    {"static", Token::KEYWORD_STATIC},
    {"sizeof", Token::KEYWORD_SIZEOF},
    {"_Alignof", Token::KEYWORD_ALIGNOF},
    {"signed", Token::KEYWORD_SIGNED},
    {"short", Token::KEYWORD_SHORT},
    {"return", Token::KEYWORD_RETURN},
    {"restrict", Token::KEYWORD_RESTRICT},
    {"register", Token::KEYWORD_REGISTER},
    {"long", Token::KEYWORD_LONG},
    {"int", Token::KEYWORD_INT},
    {"inline", Token::KEYWORD_INLINE},
    {"if", Token::KEYWORD_IF},
    {"goto", Token::KEYWORD_GOTO},
    {"for", Token::KEYWORD_FOR},
    {"float", Token::KEYWORD_FLOAT},
    {"extern", Token::KEYWORD_EXTERN},
    {"enum", Token::KEYWORD_ENUM},
    {"else", Token::KEYWORD_ELSE},
    {"double", Token::KEYWORD_DOUBLE},
    {"do", Token::KEYWORD_DO},
    {"default", Token::KEYWORD_DEFAULT},
    {"continue", Token::KEYWORD_CONTINUE},
    {"const", Token::KEYWORD_CONST},
    {"char", Token::KEYWORD_CHAR},
    {"case", Token::KEYWORD_CASE},
    {"break", Token::KEYWORD_BREAK},
    {"auto", Token::KEYWORD_AUTO},
    {"_Bool", Token::KEYWORD_BOOL},
};

std::unordered_map<std::string_view, PPDirective> PPLexer::ppKeywords = {
    {"if", PPDirective::IF},           {"ifdef", PPDirective::IFDEF},
    {"ifndef", PPDirective::IFNDEF},   {"elif", PPDirective::ELIF},
    {"else", PPDirective::ELSE},       {"endif", PPDirective::ENDIF},
    {"include", PPDirective::INCLUDE}, {"define", PPDirective::DEFINE},
    {"undef", PPDirective::UNDEF},     {"line", PPDirective::LINE},
    {"error", PPDirective::ERROR},     {"pragma", PPDirective::PRAGMA},
};

void PPLexer::getToken() {
  do {
    do {
      handleAllPPDirectives();
      replacementCtx.replaceOnce();
    } while (replacementCtx.out.empty());
    currTok = replacementCtx.out[0];
    replacementCtx.out.clear();
  } while (currTok.isWhiteSpace());
  probeKeyword();
#ifdef PPLEXER_DEBUG
  std::cerr << "[Debug][PPLexer] " << currTok << "\n";
#endif
}

bool PPSymbol::addParam(std::string_view paramName) {
  auto [it, succ] = paramIndex.try_emplace(paramName, params.size());
  if (!succ)
    return false;
  params.push_back(paramName);
  return true;
}

PPSymbol *PPSymbolTable::defineSymbol(PPSymbol &&sym) {
  auto [it, succ] = symbols.try_emplace(sym.name, std::move(sym));
  if (!succ)
    return nullptr;

  return &it->second;
}

bool PPSymbolTable::undefSymbol(std::string_view name) {
  return symbols.erase(name);
}

PPSymbol *PPSymbolTable::getSymbol(std::string_view name) {
  auto it = symbols.find(name);
  if (it == symbols.end())
    return nullptr;
  return &it->second;
}

std::optional<PPTranslationUnit>
PPTranslationUnit::load(std::filesystem::path filePath) {
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

std::optional<std::string_view> PPTranslationUnit::getLine(size_t n) {
  if (n >= newlines.size()) {
    return std::nullopt;
  }
  const char *end = newlines[n] - 1;
  const char *begin = n > 0 ? newlines[n - 1] + 1 : content.data();
  return std::string_view(begin, end);
}

std::optional<PPTranslationUnit::LineRange>
PPTranslationUnit::getLineRange(std::string_view str) {
  auto itBegin = std::lower_bound(newlines.begin(), newlines.end(), str.data());
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

Token IncludeLexer::next() {
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

bool IncludeLexer::includeHeaderName(std::string_view headerName,
                                     bool enableLocal) {
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

bool IncludeLexer::includePath(std::filesystem::path path) {
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

std::optional<IncludeLexer::LineRange>
IncludeLexer::getLineRange(std::string_view str) {
  for (auto &[_, tu] : translationUnits) {
    auto r = tu.getLineRange(str);
    if (r) {
      return LineRange{tu, r.value()};
    }
  }
  return std::nullopt;
}

bool PPLexer::handlePPDirective() {
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
      ifStack.push_back({});
    }
    lex.drop();
    break;
  }
  case PPDirective::IF: {
    if (isGroupEnabled()) {
      auto expr = parseConstExpression();
      if (!expr) {
        error("Expected expression in #if");
        return false;
      }
      PrintAST(**expr);
      bool isTrue = true;
      ifStack.push_back({isTrue, isTrue});
    } else {
      ifStack.push_back({});
      lex.dropLine();
      return true;
    }
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
    auto g = ifStack.back();
    ifStack.pop_back();
    if (isGroupEnabled()) {
      auto expr = parseConstExpression();
      if (!expr) {
        error("Expected expression in #elif");
        return false;
      }
      PrintAST(**expr);
      bool isTrue = !g.wasEnabled && false;
      ifStack.push_back({isTrue, g.wasEnabled || isTrue, false});
    } else {
      ifStack.push_back({});
      lex.dropLine();
      return true;
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

bool PPLexer::isGroupEnabled() {
  return ifStack.empty() || ifStack.back().enableGroup;
}

std::vector<Token> PPLexer::ReplacementCtx::gobbleArgument() {
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

void PPLexer::ReplacementCtx::replaceAll() {
  while (!lex.matchPeek(Token::END) && !lex.matchPeek(Token::INVALID)) {
    replaceOnce();
  }
}

void PPLexer::ReplacementCtx::replaceOnce() {
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
      ctx.lex.tokStack.insert(ctx.lex.tokStack.end(), arg.rbegin(), arg.rend());
      ctx.replaceAll();
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

void PPLexer::errorExpectedToken(std::string_view str) {
  std::cerr << "[Error][Preprocessing] Expected " << str << ", but got "
            << lex.peek() << '\n';
  replacementCtx.emitInvalid();
}

void PPLexer::error(std::string_view str) {
  std::cerr << "[Error][Preprocessing] " << str << '\n';
  replacementCtx.emitInvalid();
}

void PPLexer::errorExpectedToken(Token::Kind expectedKind) {
  errorExpectedToken(std::format("'{}'", Token::kindName(expectedKind)));
}

void PPLexer::handleAllPPDirectives() {
  while (handlePPDirective()) {
  }
}

std::vector<Token> PPLexer::gobbleReplacementList() {
  std::vector<Token> res;
  lex.skipWhiteSpaceExceptNewLine();
  while (!lex.peek().isNewLine() && !lex.peek().isEnd()) {
    res.push_back(lex.next());
  }
  trimWhiteSpaceBack(res);
  return res;
}

void PPLexer::probeKeyword() {
  if (currTok.kind != Token::IDENTIFIER) {
    return;
  }
  auto it = keywords.find(std::string_view(currTok));
  if (it == keywords.end())
    return;
  currTok.kind = it->second;
}

PPLexer::PPLexer(ASTContext &ctx, IncludeLexer &incLex, PPSymbolTable &sym)
    : ctx(ctx), incLex(incLex), lex(incLex), sym(sym) {
  lex.next();
  getToken();
}

void PPLexer::printErrCtx(std::ostream &os, std::string_view str) {
  os << "Source context: ";
  auto r = incLex.getLineRange(str);
  if (!r) {
    os << "(unavailable)\n";
    return;
  }
  os << r.value().tu.path << ":" << r.value().range.begin << "-"
     << r.value().range.end;
}

ASTPtrResult PPLexer::parseConstExpression() {
  auto toks = gobbleReplacementList();
  SymbolTable exprSym;
  IncludeLexer exprIncLex;
  PPLexer exprLex(ctx, exprIncLex, sym);
  exprLex.replacementCtx.lex.tokStack.insert(
      exprLex.replacementCtx.lex.tokStack.end(), toks.rbegin(), toks.rend());
  exprLex.drop();
  Parser exprParser(ctx, exprLex, exprSym);
  auto expr = exprParser.parseExpression();
  return expr;
}
} // namespace c
