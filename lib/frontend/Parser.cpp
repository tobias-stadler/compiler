#include <frontend/Parser.h>

AST *Parser::parsePrimary() {
  Token::Kind tokKind = lex->peekTokenKind();
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    AST *ex = parseExpression();
    // TODO: error
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      delete ex;
      return error("Expected closing paren", lex->peekToken());
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
      return nullptr;
    }
    lex->dropToken();
    auto *subExpr = parsePrimary();
    if (!subExpr) {
      return error("Expected expression after unary operator");
    }
    return new UnopAST(unopKind, subExpr);
  }
  }
}

AST *Parser::parseExpression(int prec) {
  AST *lhs = parsePrimary();
  if (!lhs) {
    return nullptr;
  }
  while (true) {
    Token::Kind tokKind = lex->peekTokenKind();
    AST::Kind binopKind = tokenBinopKind(tokKind);
    int tokPrec = binopPrecedence(binopKind);
    if (binopKind == AST::EMPTY || tokPrec < prec) {
      return lhs;
    }
    lex->dropToken();
    AST *rhs =
        parseExpression(precedenceRightAssoc(tokPrec) ? tokPrec : tokPrec + 1);
    if (!rhs) {
      delete lhs;
      return error("Expected expression after binop");
    }
    lhs = new BinopAST(binopKind, lhs, rhs);
  }
}

AST *Parser::parseStatement() {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_CURLYO: {
    lex->dropToken();
    auto *st = new CompoundStAST();
    for (AST *subSt; (subSt = parseStatement());) {
      st->children.push_back(subSt);
    }
    if (!lex->matchNextToken(Token::PUNCT_CURLYC)) {
      delete st;
      return error("Expected closing curly after compund statement");
    }
    return st;
  }
  case Token::KEYWORD_IF: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
      return error("Expected opening paren after if keyword");
    }
    auto *expr = parseExpression();
    if (!expr) {
      return error("Expected expression after if keyword");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      delete expr;
      return error("Expected closing paren after if expression");
    }
    auto *st = parseStatement();
    if (!st) {
      delete expr;
      return error("Expected statment after if");
    }
    AST *stElse = nullptr;
    if (lex->matchNextToken(Token::KEYWORD_ELSE) &&
        !(stElse = parseStatement())) {
      delete expr;
      delete st;
      return error("Expected statement after else");
    }
    return new IfStAST(expr, st, stElse);
  }
  case Token::KEYWORD_WHILE: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
      return error("Expected opening paren after while keyword");
    }
    auto *expr = parseExpression();
    if (!expr) {
      return error("Expected expression after while keyword");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      delete expr;
      return error("Expected closing paren after while expression");
    }
    auto *st = parseStatement();
    if (!st) {
      delete expr;
      return error("Expected statment after while");
    }
    return new WhileStAST(expr, st);
  }
  default: {
    auto *st = parseExpression();
    if (!st) {
      return nullptr;
    }
    if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
      delete st;
      return error("Expected semicolon after expression statement");
    }
    return st;
  }
  }
}

AST *Parser::error(std::string_view str, Token tok) {
  std::cerr << "[Error][Parser] " << str;
  if (!tok.isInvalid()) {
    if (tok.isEmpty()) {
      tok = lex->nextToken();
    }
    std::cerr << "; Exceptional Token: " << tok;
  }
  std::cerr << '\n';
  return nullptr;
}

AST *Parser::parseLiteralNum() {
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

AST *Parser::parseDeclarator() {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_STAR: {
    auto *decl = parseDeclarator();
    if (!decl) {
      return error("Expected decl after ptr-decl");
    }
    return new PtrDeclAST(decl);
  }
  case Token::IDENTIFIER: {
    return new IdentDeclAST(std::string_view(lex->nextToken()));
  }
  case Token::PUNCT_PARENO: {
    auto *decl = parseDeclarator();
    if (!decl) {
      return error("Expected decl after opening paren");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      delete decl;
      return error("Expected closing paren");
    }
    return decl;
  }
  default:
    return nullptr;
  }
}

AST *Parser::parseTypeSpec() {
  switch (lex->peekTokenKind()) {
  default:
    return nullptr;
  case Token::KEYWORD_CHAR:
    break;
  case Token::KEYWORD_SHORT:
    break;
  case Token::KEYWORD_INT:
    break;
  case Token::KEYWORD_LONG:
    break;
  }

  return nullptr;
}

AST *Parser::parseDeclaration() { return nullptr; }
