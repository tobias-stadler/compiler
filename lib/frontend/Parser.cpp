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
    return parseCompoundStatement();
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
      if (!lex->matchPeekToken(Token::PUNCT_PARENC)) {
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
          func->addNamedParam(decl->ident, std::move(decl->type));
        } while (lex->matchNextToken(Token::PUNCT_COMMA));
      }
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
    auto quali = Type::Qualifier();
    auto spec = parseDeclSpec<false, true, false>();
    if (spec) {
      quali = spec->qualifier;
    } else if (!spec.isNop()) {
      return error("Expected qualifier after ptr-decl");
    }

    auto decl = parseDeclarator(abstract);
    if (!decl) {
      return error("Expected decl after ptr-decl");
    }
    decl->spliceEnd(DeclaratorAST(make_counted<PtrType>(quali)));
    return decl;
  }
  default:
    return parseDirectDeclarator(abstract);
  }
}

ASTPtrResult Parser::parseDeclaration(bool external) {
  auto spec = parseDeclSpec();
  if (spec.isNop()) {
    return nop();
  }
  if (!spec) {
    return error("Expected declaration specifiers");
  }

  CountedPtr<Type> type = spec->createType();
  auto decl = parseDeclarator(false);
  if (!decl) {
    return error("Expected valid declarator");
  }
  decl->spliceEnd(DeclaratorAST(type, nullptr));
  if (nextIsDeclarationList()) {
    if (!external) {
      return error("Function definition not allowed here");
    }
    if (decl->type->getKind() != Type::FUNC) {
      return error("Function declarator must declare function");
    }
    auto st = parseCompoundStatement();
    if (!st) {
      return error("Expected compound statement in function definition");
    }
    return new FunctionDefinitionAST(spec.moveRes(), decl.moveRes(),
                                     st.moveRes());
  }
  auto res = std::make_unique<DeclarationAST>();
  res->declarators.push_back(decl.moveRes());
  while (lex->matchNextToken(Token::PUNCT_COMMA)) {
    auto decl = parseDeclarator(false);
    if (!decl) {
      return error("Expected valid declarator");
    }
    decl->spliceEnd(DeclaratorAST(type, nullptr));
    res->declarators.push_back(std::move(*decl));
  }
  if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
    return error("Expected semicolon after declaration");
  }
  return res;
}

ASTPtrResult Parser::parseTranslationUnit() {
  auto tu = std::make_unique<TranslationUnitAST>();
  while (true) {
    auto decl = parseDeclaration(true);
    if (decl) {
      tu->declarations.push_back(decl);
    } else if (decl.isNop()) {
      return tu;
    } else {
      return error("Expected external declaration");
    }
  }
}

ASTPtrResult Parser::parseCompoundStatement() {
  if (lex->peekTokenKind() != Token::PUNCT_CURLYO) {
    return nop();
  }
  lex->dropToken();
  auto st = std::make_unique<CompoundStAST>();
  for (ASTPtrResult subSt; (subSt = parseBlockItem());) {
    st->children.push_back(subSt);
  }
  if (!lex->matchNextToken(Token::PUNCT_CURLYC)) {
    return error("Expected closing curly after compund statement");
  }
  return st;
}

ASTPtrResult Parser::parseBlockItem() {
  auto decl = parseDeclaration();
  if (decl) {
    return decl;
  }
  if (!decl.isNop()) {
    return error("Expected declaration");
  }
  auto st = parseStatement();
  if (st) {
    return st;
  }
  if (!st.isNop()) {
    return error("Expected statement");
  }
  return nop();
}

bool Parser::nextIsAbstractDeclarator() {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_STAR:
  case Token::PUNCT_PARENO:
  case Token::PUNCT_SQUAREO:
    return true;
  default:
    return false;
  }
}
