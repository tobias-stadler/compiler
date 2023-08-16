#include "frontend/AST.h"
#include "frontend/Lexer.h"
#include "frontend/Type.h"
#include "support/RefCount.h"
#include <array>
#include <frontend/Parser.h>
#include <memory>
#include <string_view>

ASTPtrResult Parser::parsePrimary() {
  Token::Kind tokKind = lex->peekTokenKind();
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    auto ex = parseExpression();
    if (!ex) {
      return error("Expected expresssion");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      return error("Expected closing paren");
    }
    return ex;
  }
  case Token::IDENTIFIER: {
    return new VarAST(std::string_view(lex->nextToken()));
  }
  case Token::LITERAL_NUM:
    return parseLiteralNum();
  default: {
    AST::Kind unopKind = tokenUnopKind(tokKind);
    if (unopKind == AST::EMPTY) {
      return nop();
    }
    lex->dropToken();
    auto subExpr = parsePrimary();
    if (!subExpr) {
      return error("Expected expression after unary operator");
    }
    return new UnopAST(unopKind, subExpr);
  }
  }
}

ASTPtrResult Parser::parseExpression(int prec) {
  auto lhs = parsePrimary();
  if (!lhs) {
    return nop();
  }
  while (true) {
    Token::Kind tokKind = lex->peekTokenKind();
    AST::Kind binopKind = tokenBinopKind(tokKind);
    int tokPrec = binopPrecedence(binopKind);
    if (binopKind == AST::EMPTY || tokPrec < prec) {
      return lhs;
    }
    lex->dropToken();
    auto rhs =
        parseExpression(precedenceRightAssoc(tokPrec) ? tokPrec : tokPrec + 1);
    if (!rhs) {
      return error("Expected expression after binop");
    }
    lhs = new BinopAST(binopKind, lhs, rhs);
  }
}

ASTPtrResult Parser::parseStatement() {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_CURLYO: {
    lex->dropToken();
    auto st = std::make_unique<CompoundStAST>();
    for (ASTPtrResult subSt; (subSt = parseStatement());) {
      st->children.push_back(subSt);
    }
    if (!lex->matchNextToken(Token::PUNCT_CURLYC)) {
      return error("Expected closing curly after compund statement");
    }
    return st;
  }
  case Token::KEYWORD_IF: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
      return error("Expected opening paren after if keyword");
    }
    auto expr = parseExpression();
    if (!expr) {
      return error("Expected expression after if keyword");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      return error("Expected closing paren after if expression");
    }
    auto st = parseStatement();
    if (!st) {
      return error("Expected statment after if");
    }
    if (lex->matchNextToken(Token::KEYWORD_ELSE)) {
      auto stElse = parseStatement();
      if (!stElse) {
        return error("Expected statement after else");
      }
      return new IfStAST(expr, st, stElse);
    }
    return new IfStAST(expr, st);
  }
  case Token::KEYWORD_WHILE: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
      return error("Expected opening paren after while keyword");
    }
    auto expr = parseExpression();
    if (!expr) {
      return error("Expected expression after while keyword");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      return error("Expected closing paren after while expression");
    }
    auto st = parseStatement();
    if (!st) {
      return error("Expected statment after while");
    }
    return new WhileStAST(expr, st);
  }
  default: {
    auto st = parseExpression();
    if (!st) {
      return nop();
    }
    if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
      return error("Expected semicolon after expression statement");
    }
    return st;
  }
  }
}

ASTError Parser::error(std::string_view str, Token tok) {
  std::cerr << "[Error][Parser] " << str;
  if (tok.isEmpty()) {
    tok = lex->peekToken();
  }
  std::cerr << "; Exceptional Token: " << tok;
  std::cerr << '\n';
  return ASTError(ASTError::EXPECTED_TOKEN);
}

ASTPtrResult Parser::parseLiteralNum() {
  Token tok = lex->nextToken();
  if (tok.kind != Token::LITERAL_NUM) {
    return error("Expected literal num", tok);
  }
  std::string_view str(tok);
  int64_t num;
  auto res = std::from_chars(str.data(), str.data() + str.size(), num);
  if (res.ec != std::errc{}) {
    return error("Invalid num", tok);
  }
  return new NumAST(num);
}

Parser::Parser(Lexer *lex) : lex(lex) { assert(lex); }

ASTResult<DeclaratorAST> Parser::parseSingleDirectDeclarator(bool abstract,
                                                             bool first) {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_PARENO: {
    lex->dropToken();
    if (abstract ? nextIsAbstractDeclarator() : first) {
      auto decl = parseDeclarator(abstract);
      if (!decl) {
        return error("Expected decl after opening paren");
      }
      if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
        return error("Expected closing paren");
      }
      return decl;
    } else {
      auto func = make_counted<FuncType>();
      do {
        auto spec = parseDeclSpec<true, true, false>();
        if (!spec) {
          return error("Expected decl specifiers");
        }
        auto decl = parseDeclarator(false);
        if (!decl) {
          return error("Expected declarator");
        }
        decl->spliceEnd(DeclaratorAST(spec->createType(), nullptr));
        func->addParam(std::move(decl->type));
      } while (lex->matchNextToken(Token::PUNCT_COMMA));
      if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
        return error("Expected closing paren");
      }
      return DeclaratorAST(std::move(func));
    }
  }
  case Token::IDENTIFIER: {
    if (abstract) {
      return nop();
    } else {
      return DeclaratorAST(nullptr, nullptr,
                           std::string_view(lex->nextToken()));
    }
  }
  default:
    return nop();
  }
}

ASTResult<DeclaratorAST> Parser::parseDirectDeclarator(bool abstract) {
  DeclaratorAST res(nullptr, nullptr);
  bool first = true;
  while (true) {
    auto decl = parseSingleDirectDeclarator(abstract, first);
    if (decl) {
      res.spliceEnd(*decl);
    } else if (decl.isNop()) {
      if (first) {
        return nop();
      }
      return res;
    } else {
      return error("Invalid direct declarator");
    }
    first = false;
  }
}

ASTResult<DeclaratorAST> Parser::parseDeclarator(bool abstract) {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_STAR: {
    lex->dropToken();
    auto spec = parseDeclSpec<false, true, false>();
    if (!spec) {
      return error("Expected qualifier after ptr-decl");
    }
    auto decl = parseDeclarator(abstract);
    if (!decl) {
      return error("Expected decl after ptr-decl");
    }
    decl->spliceEnd(DeclaratorAST(make_counted<PtrType>(spec->qualifier)));
    return decl;
  }
  default:
    return parseDirectDeclarator(abstract);
  }
}

ASTPtrResult Parser::parseDeclaration() {
  auto spec = parseDeclSpec();
  if (!spec) {
    return error("Expected declaration specifiers");
  }
  auto res = std::make_unique<DeclarationAST>();
  CountedPtr<Type> type = spec->createType();
  do {
    auto decl = parseDeclarator(true);
    if (!decl) {
      return error("Expected valid declarator");
    }
    decl->spliceEnd(DeclaratorAST(type, nullptr));
  } while (lex->matchNextToken(Token::PUNCT_COMMA));
  if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
    return error("Expected semicolon after declaration");
  }
  return res;
}
