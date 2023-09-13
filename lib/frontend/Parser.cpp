#include "frontend/AST.h"
#include "frontend/Lexer.h"
#include "frontend/Type.h"
#include "support/RefCount.h"
#include <array>
#include <cassert>
#include <charconv>
#include <frontend/Parser.h>
#include <memory>
#include <string_view>

namespace {
constexpr AST::Kind tokenBinopKind(Token::Kind kind) {
  switch (kind) {
  default:
    return AST::EMPTY;
  case Token::PUNCT_EQ:
    return AST::ASSIGN;
  case Token::PUNCT_PLUSEQ:
    return AST::ASSIGN_ADD;
  case Token::PUNCT_MINUSEQ:
    return AST::ASSIGN_SUB;
  case Token::PUNCT_STAREQ:
    return AST::ASSIGN_MUL;
  case Token::PUNCT_SLASHEQ:
    return AST::ASSIGN_DIV;
  case Token::PUNCT_PERCENTEQ:
    return AST::ASSIGN_MOD;
  case Token::PUNCT_ANDEQ:
    return AST::ASSIGN_AND;
  case Token::PUNCT_OREQ:
    return AST::ASSIGN_OR;
  case Token::PUNCT_XOREQ:
    return AST::ASSIGN_XOR;
  case Token::PUNCT_LTLTEQ:
    return AST::ASSIGN_LSHIFT;
  case Token::PUNCT_GTGTEQ:
    return AST::ASSIGN_RSHIFT;
  case Token::PUNCT_PLUS:
    return AST::ADD;
  case Token::PUNCT_MINUS:
    return AST::SUB;
  case Token::PUNCT_STAR:
    return AST::MUL;
  case Token::PUNCT_SLASH:
    return AST::DIV;
  case Token::PUNCT_PERCENT:
    return AST::MOD;
  case Token::PUNCT_AND:
    return AST::BIT_AND;
  case Token::PUNCT_OR:
    return AST::BIT_OR;
  case Token::PUNCT_XOR:
    return AST::BIT_XOR;
  case Token::PUNCT_LTLT:
    return AST::LSHIFT;
  case Token::PUNCT_GTGT:
    return AST::RSHIFT;
  case Token::PUNCT_ANDAND:
    return AST::LOG_AND;
  case Token::PUNCT_OROR:
    return AST::LOG_OR;
  case Token::PUNCT_EQEQ:
    return AST::EQ;
  case Token::PUNCT_EXCLAMATIONEQ:
    return AST::NEQ;
  case Token::PUNCT_LT:
    return AST::LT;
  case Token::PUNCT_GT:
    return AST::GT;
  case Token::PUNCT_LTEQ:
    return AST::LTE;
  case Token::PUNCT_GTEQ:
    return AST::GTE;
  }
}

constexpr AST::Kind tokenUnopKind(Token::Kind kind) {
  switch (kind) {
  default:
    return AST::EMPTY;
  case Token::PUNCT_STAR:
    return AST::DEREF;
  case Token::PUNCT_AND:
    return AST::ADDR;
  case Token::PUNCT_EXCLAMATION:
    return AST::LOG_NOT;
  case Token::PUNCT_TILDE:
    return AST::BIT_NOT;
  case Token::PUNCT_PLUSPLUS:
    return AST::INC_PRE;
  case Token::PUNCT_MINUSMINUS:
    return AST::DEC_PRE;
  }
}

constexpr int binopPrecedence(AST::Kind kind) {
  switch (kind) {
  case AST::MUL:
  case AST::DIV:
    return 12;
  case AST::ADD:
  case AST::SUB:
    return 11;
  case AST::LSHIFT:
  case AST::RSHIFT:
    return 10;
  case AST::LT:
  case AST::GT:
  case AST::LTE:
  case AST::GTE:
    return 9;
  case AST::EQ:
  case AST::NEQ:
    return 8;
  case AST::BIT_AND:
    return 7;
  case AST::BIT_XOR:
    return 6;
  case AST::BIT_OR:
    return 5;
  case AST::LOG_AND:
    return 4;
  case AST::LOG_OR:
    return 3;
  case AST::ASSIGN:
  case AST::ASSIGN_ADD:
  case AST::ASSIGN_SUB:
  case AST::ASSIGN_MUL:
  case AST::ASSIGN_DIV:
  case AST::ASSIGN_MOD:
  case AST::ASSIGN_LSHIFT:
  case AST::ASSIGN_RSHIFT:
  case AST::ASSIGN_AND:
  case AST::ASSIGN_OR:
  case AST::ASSIGN_XOR:
    return 1;
  default:
    return -1;
  }
}

constexpr bool precedenceRightAssoc(int prec) {
  switch (prec) {
  default:
    return false;
  case 1:
    return true;
  }
}
} // namespace

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
    if (unopKind == AST::ADDR && (*subExpr)->getKind() == AST::VAR) {
      auto *s = sym->getSymbol(static_cast<VarAST &>(*subExpr->get()).ident);
      if (s) {
        s->setAddrTaken(true);
      }
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
    decl->spliceEnd(DeclaratorAST(make_counted<PtrType>(nullptr, quali)));
    return decl;
  }
  default:
    return parseDirectDeclarator(abstract);
  }
}

ASTPtrResult Parser::parseDeclaration() {
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
    // Parsing function definition
    if (!sym->scope().isFile()) {
      return error("Function definition only allowed in file scope");
    }
    if (decl->type->getKind() != Type::FUNC) {
      return error("Function declarator must declare function");
    }

    auto *s =
        sym->declareSymbol(decl->ident, Symbol(Symbol::EXTERN, decl->type));
    // TODO: prototype support
    if (!s) {
      return error("Function redefined");
    }

    auto ast =
        std::make_unique<FunctionDefinitionAST>(spec.moveRes(), decl.moveRes());
    sym->pushScope(ast->funcScope);
    sym->pushScope(ast->blockScope);
    auto st = parseCompoundStatement();
    if (!st) {
      return error("Expected compound statement in function definition");
    }
    ast->st = st.moveRes();
    sym->popScope();
    sym->popScope();

    return ast;
  }

  auto ast = std::make_unique<DeclarationAST>();
  ast->declarators.push_back(decl.moveRes());
  while (lex->matchNextToken(Token::PUNCT_COMMA)) {
    auto decl = parseDeclarator(false);
    if (!decl) {
      return error("Expected valid declarator");
    }
    decl->spliceEnd(DeclaratorAST(type, nullptr));
    ast->declarators.push_back(std::move(*decl));
  }
  if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
    return error("Expected semicolon after declaration");
  }

  if (sym->scope().isFile()) {
    // TODO: global variable declaration in symbol table
    assert(false && "global variable declaration unsupported");
  } else {
    for (auto &d : ast->declarators) {
      // TOOD: proper handling of storage spec
      auto *s = sym->declareSymbol(d.ident, Symbol(Symbol::AUTO, d.type));
      if (!s) {
        return error("Symbol redeclared");
      }
    }
  }
  return ast;
}

ASTResult<std::unique_ptr<TranslationUnitAST>> Parser::parseTranslationUnit() {
  auto tu = std::make_unique<TranslationUnitAST>();
  sym->pushScope(tu->scope);
  while (true) {
    auto declRes = parseDeclaration();
    if (!declRes) {
      if (declRes.isNop()) {
        sym->popScope();
        return tu;
      } else {
        return error("Expected external declaration");
      }
    }
    tu->declarations.push_back(declRes.moveRes());
  }
}

ASTPtrResult Parser::parseCompoundStatement() {
  if (lex->peekTokenKind() != Token::PUNCT_CURLYO) {
    return nop();
  }
  lex->dropToken();
  auto st = std::make_unique<CompoundStAST>();
  sym->pushScope(st->scope);
  for (ASTPtrResult subSt; (subSt = parseBlockItem());) {
    st->children.push_back(subSt);
  }
  if (!lex->matchNextToken(Token::PUNCT_CURLYC)) {
    return error("Expected closing curly after compound statement");
  }
  sym->popScope();
  return st;
}

ASTPtrResult Parser::parseBlockItem() {
  auto declRes = parseDeclaration();
  if (declRes) {
    return declRes;
  }
  if (!declRes.isNop()) {
    return error("Expected declaration");
  }

  auto stRes = parseStatement();
  if (stRes) {
    return stRes;
  }
  if (!stRes.isNop()) {
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

bool Parser::nextIsDeclarationList() {
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_EQ:
  case Token::PUNCT_COMMA:
  case Token::PUNCT_SEMICOLON:
    return false;
  default:
    return true;
  }
}
