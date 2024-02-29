#include "c/Parser.h"
#include "c/AST.h"
#include "c/Lexer.h"
#include "c/Symbol.h"
#include "c/Type.h"
#include "support/RefCount.h"
#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <memory>
#include <string>
#include <string_view>

#define P_ERR_CTX(x) auto log_err = log.logFrame(x)

#define P_AST_EXPECT(x, msg)                                                   \
  if (!x) [[unlikely]] {                                                       \
    if (x.isNop()) {                                                           \
      return error("Expected " msg);                                           \
    }                                                                          \
    return x.err();                                                            \
  }

#define P_AST_EXPECT_OR_NOP(x)                                                 \
  if (!x) [[unlikely]] {                                                       \
    if (!x.isNop()) {                                                          \
      return x.err();                                                          \
    }                                                                          \
  }

#define P_AST_EXPECT_FWD(x)                                                    \
  if (!x) [[unlikely]] {                                                       \
    return x.err();                                                            \
  }

#define P_TOK_EXPECT_NEXT(x)                                                   \
  if (!lex->matchNextToken(x)) [[unlikely]] {                                  \
    return errorExpectedToken(x);                                              \
  }

#define P_TOK_EXPECT_PEEK(x)                                                   \
  if (!lex->matchPeekToken(x)) [[unlikely]] {                                  \
    return errorExpectedToken(x);                                              \
  }

namespace c {

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
  case Token::PUNCT_PLUS:
    return AST::PLUS;
  case Token::PUNCT_MINUS:
    return AST::MINUS;
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

auto getTypeSpecSets() {
  std::vector<std::pair<std::vector<Token::Kind>, Type::Kind>> typeSpecSets{
      {{Token::KEYWORD_VOID}, Type::VOID},
      {{Token::KEYWORD__BOOL}, Type::BOOL},

      {{Token::KEYWORD_CHAR}, Type::SCHAR},
      {{Token::KEYWORD_CHAR, Token::KEYWORD_SIGNED}, Type::SCHAR},
      {{Token::KEYWORD_CHAR, Token::KEYWORD_UNSIGNED}, Type::UCHAR},

      {{Token::KEYWORD_SHORT}, Type::SSHORT},
      {{Token::KEYWORD_SHORT, Token::KEYWORD_SIGNED}, Type::SSHORT},
      {{Token::KEYWORD_SHORT, Token::KEYWORD_SIGNED, Token::KEYWORD_INT},
       Type::SSHORT},
      {{Token::KEYWORD_SHORT, Token::KEYWORD_UNSIGNED}, Type::USHORT},
      {{Token::KEYWORD_SHORT, Token::KEYWORD_UNSIGNED, Token::KEYWORD_INT},
       Type::USHORT},

      {{Token::KEYWORD_INT}, Type::SINT},
      {{Token::KEYWORD_SIGNED}, Type::SINT},
      {{Token::KEYWORD_INT, Token::KEYWORD_SIGNED}, Type::SINT},
      {{Token::KEYWORD_UNSIGNED}, Type::UINT},
      {{Token::KEYWORD_INT, Token::KEYWORD_UNSIGNED}, Type::UINT},

      {{Token::KEYWORD_LONG}, Type::SLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_SIGNED}, Type::SLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_SIGNED, Token::KEYWORD_INT},
       Type::SLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_UNSIGNED}, Type::ULONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_UNSIGNED, Token::KEYWORD_INT},
       Type::ULONG},

      {{Token::KEYWORD_LONG, Token::KEYWORD_LONG}, Type::SLONGLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_LONG, Token::KEYWORD_SIGNED},
       Type::SLONGLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_LONG, Token::KEYWORD_SIGNED,
        Token::KEYWORD_INT},
       Type::SLONGLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_LONG, Token::KEYWORD_UNSIGNED},
       Type::ULONGLONG},
      {{Token::KEYWORD_LONG, Token::KEYWORD_LONG, Token::KEYWORD_UNSIGNED,
        Token::KEYWORD_INT},
       Type::ULONGLONG},
  };
  for (auto &[toks, _] : typeSpecSets) {
    std::sort(toks.begin(), toks.end());
  }
  return std::map<std::vector<Token::Kind>, Type::Kind>(typeSpecSets.begin(),
                                                        typeSpecSets.end());
}

auto typeSpecSets = getTypeSpecSets();

} // namespace

ASTPtrResult Parser::parseUnary() {
  Token::Kind tokKind = lex->peekTokenKind();
  AST::Ptr res;
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    lex->dropToken();
    auto ex = parseExpression();
    P_AST_EXPECT(ex, "expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    res = ex.moveRes();
    break;
  }
  case Token::IDENTIFIER: {
    res = std::make_unique<VarAST>(std::string_view(lex->nextToken()));
    break;
  }
  case Token::LITERAL_NUM:
    res = parseLiteralNum();
    break;
  default: {
    AST::Kind unopKind = tokenUnopKind(tokKind);
    if (unopKind == AST::EMPTY) {
      return nop();
    }
    lex->dropToken();
    auto subExpr = parseUnary();
    P_AST_EXPECT(subExpr, "expression after unary operator");
    if (unopKind == AST::ADDR && (*subExpr)->getKind() == AST::VAR) {
      auto *s = sym->getSymbol(static_cast<VarAST &>(*subExpr->get()).ident,
                               Symbol::Namespace::ORDINARY);
      if (s) {
        s->setAddrTaken(true);
      }
    }
    res = std::make_unique<UnopAST>(unopKind, subExpr);
    break;
  }
  }
  return parsePostfix(std::move(res));
}

ASTPtrResult Parser::parseExpression(int prec) {
  auto lhs = parseUnary();
  P_AST_EXPECT_FWD(lhs);
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
    P_AST_EXPECT(rhs, "expression");
    lhs = new BinopAST(binopKind, lhs, rhs);
  }
}

ASTPtrResult Parser::parseStatement() {
  Token::Kind tokKind = lex->peekTokenKind();
  switch (tokKind) {
  case Token::PUNCT_SEMICOLON: {
    return error("Stray semicolon");
  }
  case Token::PUNCT_CURLYO: {
    return parseCompoundStatement();
  }
  case Token::KEYWORD_IF: {
    P_ERR_CTX(ErrCtx::ST_IF);
    lex->dropToken();
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "condition expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    auto st = parseStatement();
    P_AST_EXPECT(st, "statement");
    if (lex->matchNextToken(Token::KEYWORD_ELSE)) {
      auto stElse = parseStatement();
      P_AST_EXPECT(stElse, "else statement");
      return new IfStAST(expr, st, stElse);
    }
    return new IfStAST(expr, st);
  }
  case Token::KEYWORD_WHILE: {
    P_ERR_CTX(ErrCtx::ST_WHILE);
    lex->dropToken();
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "condition expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    auto st = parseStatement();
    P_AST_EXPECT(st, "statement");
    return new WhileStAST(expr, st);
  }
  case Token::KEYWORD_FOR: {
    P_ERR_CTX(ErrCtx::ST_FOR);
    lex->dropToken();
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto initClauseRes = parseDeclaration();
    AST::Ptr initClause = nullptr;
    if (initClauseRes.isNop()) {
      initClauseRes = parseExpression();
      P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    }
    if (initClauseRes) {
      initClause = initClauseRes.moveRes();
    } else if (!initClauseRes.isNop()) {
      return error("Expected expression or declaration in for loop");
    }

    auto exprCondRes = parseExpression();
    AST::Ptr exprCond = nullptr;
    if (exprCondRes) {
      exprCond = exprCondRes.moveRes();
    } else if (!exprCondRes.isNop()) {
      return error("Expected condition expression in for loop");
    }
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    auto exprIterRes = parseExpression();
    AST::Ptr exprIter = nullptr;
    if (exprIterRes) {
      exprIter = exprIterRes.moveRes();
    } else if (!exprIterRes.isNop()) {
      return error("Expected iter expression in for loop");
    }
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    auto stRes = parseStatement();
    P_AST_EXPECT(stRes, "statement");
    return std::make_unique<ForStAST>(std::move(initClause),
                                      std::move(exprCond), std::move(exprIter),
                                      stRes.moveRes());
  }
  case Token::KEYWORD_CONTINUE:
  case Token::KEYWORD_BREAK: {
    lex->dropToken();
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return std::make_unique<LoopCtrlStAST>(
        tokKind == Token::KEYWORD_CONTINUE ? AST::ST_CONTINUE : AST::ST_BREAK);
  }

  case Token::KEYWORD_RETURN: {
    lex->dropToken();
    auto exprRes = parseExpression();
    P_AST_EXPECT_OR_NOP(exprRes);
    AST::Ptr expr = nullptr;
    if (exprRes) {
      expr = exprRes.moveRes();
    }
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return std::make_unique<ReturnStAST>(std::move(expr));
  }
  default: {
    P_ERR_CTX(ErrCtx::ST_EXPRESSION);
    auto st = parseExpression();
    P_AST_EXPECT_FWD(st);
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return st;
  }
  }
}

ASTError Parser::error(std::string_view str) {
  log.freeze();
  std::cerr << "[Error][Parser] " << str << '\n';
  return ASTError(ASTError::OTHER);
}

ASTPtrResult Parser::parseLiteralNum() {
  Token tok = lex->nextToken();
  if (tok.kind != Token::LITERAL_NUM) {
    return errorExpectedToken(Token::LITERAL_NUM, tok);
  }
  std::string_view str(tok);
  int64_t num;
  auto res = std::from_chars(str.data(), str.data() + str.size(), num);
  if (res.ec != std::errc{}) {
    return error("Invalid num");
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
      P_AST_EXPECT(decl, "declarator");
      P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
      return decl;
    } else {
      auto func = make_counted<FuncType>();
      if (!lex->matchPeekToken(Token::PUNCT_PARENC)) {
        do {
          auto spec = parseDeclSpec(true, true, false);
          P_AST_EXPECT(spec, "declaration specifiers");
          auto decl = parseDeclarator(false);
          P_AST_EXPECT(decl, "declarator");
          decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
          func->addNamedParam(decl->ident, std::move(decl->type));
        } while (lex->matchNextToken(Token::PUNCT_COMMA));
      }
      P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
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
  P_ERR_CTX(ErrCtx::DECLARATOR);
  switch (lex->peekTokenKind()) {
  case Token::PUNCT_STAR: {
    lex->dropToken();
    auto quali = Type::Qualifier();
    auto spec = parseDeclSpec(false, true, false);
    P_AST_EXPECT_OR_NOP(spec);
    if (spec) {
      quali = spec->qualifier;
    }

    auto decl = parseDeclarator(abstract);
    P_AST_EXPECT(decl, "declarator after pointer-declarator");
    decl->spliceEnd(DeclaratorAST(make_counted<PtrType>(nullptr, quali)));
    return decl;
  }
  default:
    return parseDirectDeclarator(abstract);
  }
}

ASTPtrResult Parser::parseDeclaration() {
  P_ERR_CTX(ErrCtx::DECLARATION);
  auto spec = parseDeclSpec(true, true, true);
  P_AST_EXPECT_FWD(spec);

  auto decl = parseDeclarator(false);
  P_AST_EXPECT_OR_NOP(decl);
  if (!decl) {
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return std::make_unique<DeclarationAST>(Symbol::TYPEDEF);
  }
  decl->spliceEnd(DeclaratorAST(spec->type, nullptr));

  if (nextIsDeclarationList()) {
    P_ERR_CTX(ErrCtx::FUNC_DEF);
    // Parsing function definition
    if (!sym->scope().isFile()) {
      return error("Function definition only allowed in file scope");
    }
    if (decl->type->getKind() != Type::FUNC) {
      return error("Function declarator must declare function");
    }

    auto ast = std::make_unique<FunctionDefinitionAST>(spec->symbolKind,
                                                       decl.moveRes());
    Symbol::Kind symbolKind = sym->scope().getSymbolKind(ast->symbolKind);

    if (!(symbolKind == Symbol::EXTERN || symbolKind == Symbol::STATIC)) {
      return error("Invalid storage class for function");
    }
    auto *s = sym->declareSymbol(Symbol(symbolKind, ast->decl.type, decl->ident,
                                        Symbol::Namespace::ORDINARY));
    // TODO: prototype support
    if (!s) {
      return error("Function redefined");
    }

    sym->pushScope(ast->funcScope);
    sym->pushScope(ast->blockScope);
    for (auto [ident, ty] : ast->getType().getParams()) {
      sym->declareSymbol(
          Symbol(Symbol::AUTO, ty, ident, Symbol::Namespace::ORDINARY));
    }
    auto st = parseCompoundStatement();
    P_AST_EXPECT(st, "compound statement for function definition");
    ast->st = st.moveRes();
    sym->popScope();
    sym->popScope();

    return ast;
  }

  auto ast = std::make_unique<DeclarationAST>(spec->symbolKind);
  while (true) {
    AST::Ptr initializer = nullptr;
    if (lex->matchNextToken(Token::PUNCT_EQ)) {
      auto expr = parseExpression();
      P_AST_EXPECT(expr, "expression as initializer");
      initializer = expr.moveRes();
    }
    ast->declarators.emplace_back(decl.moveRes(), std::move(initializer));
    if (!lex->matchNextToken(Token::PUNCT_COMMA)) {
      break;
    }
    decl = parseDeclarator(false);
    P_AST_EXPECT(decl, "declarator");
    decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
  }

  P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);

  Symbol::Kind symbolKind = sym->scope().getSymbolKind(ast->symbolKind);
  // TODO: verify symbolKind

  for (auto &[d, _] : ast->declarators) {
    auto *s = sym->declareSymbol(
        Symbol(symbolKind, d.type, d.ident, Symbol::Namespace::ORDINARY));
    if (!s) {
      return error(std::string("Symbol redeclared: ") + std::string(d.ident));
    }
  }
  return ast;
}

ASTResult<std::unique_ptr<TranslationUnitAST>> Parser::parseTranslationUnit() {
  auto tu = std::make_unique<TranslationUnitAST>();
  sym->pushScope(tu->scope);
  while (true) {
    auto declRes = parseDeclaration();
    P_AST_EXPECT_OR_NOP(declRes);
    if (!declRes) {
      break;
    }
    tu->declarations.push_back(declRes.moveRes());
  }
  sym->popScope();
  return tu;
}

ASTPtrResult Parser::parseCompoundStatement() {
  if (lex->peekTokenKind() != Token::PUNCT_CURLYO) {
    return nop();
  }
  lex->dropToken();
  auto st = std::make_unique<CompoundStAST>();
  sym->pushScope(st->scope);
  while (true) {
    ASTPtrResult subSt = parseBlockItem();
    P_AST_EXPECT_OR_NOP(subSt);
    if (!subSt) {
      break;
    }
    st->children.push_back(subSt);
  }
  P_TOK_EXPECT_NEXT(Token::PUNCT_CURLYC);
  sym->popScope();
  return st;
}

ASTPtrResult Parser::parseBlockItem() {
  auto declRes = parseDeclaration();
  P_AST_EXPECT_OR_NOP(declRes);
  if (declRes) {
    return declRes;
  }

  auto stRes = parseStatement();
  P_AST_EXPECT_OR_NOP(stRes);
  if (stRes) {
    return stRes;
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

ASTResult<CountedPtr<StructType>> Parser::parseStruct() {
  P_ERR_CTX(ErrCtx::STRUCT);
  bool isUnion = false;
  bool isStruct = false;
  bool isEnum = false;
  switch (lex->peekTokenKind()) {
  case Token::KEYWORD_UNION:
    isUnion = true;
    break;
  case Token::KEYWORD_STRUCT:
    isStruct = true;
    break;
  case Token::KEYWORD_ENUM:
    isEnum = true;
    break;
  default:
    return nop();
  }
  lex->dropToken();
  std::string_view name;
  CountedPtr<StructType> res;
  if (lex->matchPeekToken(Token::IDENTIFIER)) {
    name = lex->nextToken();
    if (auto *s = sym->getSymbol(name, Symbol::Namespace::TAG)) {
      res = CountedPtr{as<StructType>(s->getType().get())};
      if (!((isUnion && res->getKind() == Type::UNION) ||
            (isStruct && res->getKind() == Type::STRUCT) ||
            isEnum && res->getKind() == Type::ENUM)) {
        return error("Redeclared tag with different specifier");
      }
    }
  }
  if (!res) {
    res = make_counted<StructType>(isUnion);
    if (!name.empty()) {
      sym->declareSymbol(
          Symbol(Symbol::TYPEDEF, res, name, Symbol::Namespace::TAG));
    }
  }
  if (!lex->matchNextToken(Token::PUNCT_CURLYO)) {
    if (name.empty()) {
      return error("Expected identifier or struct declaration list");
    }
    return res;
  }
  if (res->isComplete()) {
    return error("Struct is already complete");
  }

  while (true) {
    auto spec = parseDeclSpec(true, true, false);
    P_AST_EXPECT_OR_NOP(spec);
    if (!spec) {
      break;
    }
    do {
      auto decl = parseDeclarator(false);
      P_AST_EXPECT(decl, "declarator");
      decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
      res->addNamedMember(decl->ident, std::move(decl->type));
    } while (lex->matchNextToken(Token::PUNCT_COMMA));
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
  }

  res->setComplete(true);
  P_TOK_EXPECT_NEXT(Token::PUNCT_CURLYC);
  return res;
}

ASTResult<CountedPtr<EnumType>> Parser::parseEnum() {
  P_ERR_CTX(ErrCtx::ENUM);
  if (!lex->matchNextToken(Token::KEYWORD_ENUM)) {
    return nop();
  }
  std::string_view name;
  if (lex->matchPeekToken(Token::IDENTIFIER)) {
    name = lex->nextToken();
  }
  return error("unimplemented");
}

ASTPtrResult Parser::parsePostfix(AST::Ptr base) {
  AST::Ptr res;
  Token::Kind tokKind = lex->peekTokenKind();
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    lex->dropToken();
    auto call = std::make_unique<FunctionCallAST>(std::move(base));
    if (!lex->matchPeekToken(Token::PUNCT_PARENC)) {
      do {
        auto expr = parseExpression();
        P_AST_EXPECT(expr, "expression");
        call->args.push_back(expr.moveRes());
      } while (lex->matchNextToken(Token::PUNCT_COMMA));
    }
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    res = std::move(call);
    break;
  }
  case Token::PUNCT_SQUAREO: {
    lex->dropToken();
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_SQUAREC);
    res = std::make_unique<ArrAccessAST>(std::move(base), expr.moveRes());
    break;
  }
  case Token::PUNCT_DOT:
  case Token::PUNCT_ARROW: {
    lex->dropToken();
    P_TOK_EXPECT_PEEK(Token::IDENTIFIER);
    res = std::make_unique<MemberAccessAST>(tokKind == Token::PUNCT_ARROW
                                                ? AST::ACCESS_MEMBER_DEREF
                                                : AST::ACCESS_MEMBER,
                                            std::move(base), lex->nextToken());
    break;
  }
  case Token::PUNCT_PLUSPLUS:
  case Token::PUNCT_MINUSMINUS: {
    lex->dropToken();
    res = std::make_unique<UnopAST>(
        tokKind == Token::PUNCT_PLUSPLUS ? AST::INC_POST : AST::DEC_POST,
        std::move(base));
    break;
  }
  default:
    return base;
  }
  return parsePostfix(std::move(res));
}

ASTResult<DeclSpec> Parser::parseDeclSpec(bool enableTypeSpec,
                                          bool enableTypeQuali,
                                          bool enableStorageSpec) {
  P_ERR_CTX(ErrCtx::DECL_SPEC);
  bool isNop = true;
  Type::Qualifier qualifier = Type::Qualifier();
  Symbol::Kind storageKind = Symbol::EMPTY;
  CountedPtr<StructType> structType;
  std::vector<Token::Kind> typeSpecs;
  int qConst = 0;
  int qVolatile = 0;
  int qRestrict = 0;
  std::vector<Token::Kind> storageSpecs;
  while (true) {
    Token::Kind kind = lex->peekTokenKind();
    switch (kind) {
    default:
      goto done;
    case Token::KEYWORD_VOID:
    case Token::KEYWORD_CHAR:
    case Token::KEYWORD_SHORT:
    case Token::KEYWORD_INT:
    case Token::KEYWORD_LONG:
    case Token::KEYWORD_UNSIGNED:
    case Token::KEYWORD_SIGNED:
    case Token::KEYWORD__BOOL:
      if (enableTypeSpec) {
        typeSpecs.push_back(kind);
        lex->dropToken();
        break;
      } else {
        goto done;
      }
    case Token::KEYWORD_STRUCT:
    case Token::KEYWORD_UNION:
      if (enableTypeSpec) {
        if (structType) {
          return error("Specified multiple struct types");
        }
        auto tyRes = parseStruct();
        P_AST_EXPECT(tyRes, "struct/union");
        structType = tyRes.moveRes();
        break;
      } else {
        goto done;
      }
    case Token::KEYWORD_CONST:
      ++qConst;
      goto qualiCont;
    case Token::KEYWORD_RESTRICT:
      ++qRestrict;
      goto qualiCont;
    case Token::KEYWORD_VOLATILE:
      ++qVolatile;
    qualiCont:
      if (enableTypeQuali) {
        lex->dropToken();
        break;
      } else {
        goto done;
      }
    case Token::KEYWORD_TYPEDEF:
    case Token::KEYWORD_EXTERN:
    case Token::KEYWORD_STATIC:
    case Token::KEYWORD_AUTO:
    case Token::KEYWORD_REGISTER:
      if (enableStorageSpec) {
        storageSpecs.push_back(kind);
        lex->dropToken();
        break;
      } else {
        goto done;
      }
    }
    isNop = false;
  }
done:
  if (isNop) {
    return nop();
  }
  if (enableStorageSpec) {
    if (storageSpecs.size() == 0) {
      storageKind = Symbol::EMPTY;
    } else if (storageSpecs.size() == 1) {
      switch (storageSpecs[0]) {
      default:
        break;
      case Token::KEYWORD_TYPEDEF:
        storageKind = Symbol::TYPEDEF;
        break;
      case Token::KEYWORD_EXTERN:
        storageKind = Symbol::EXTERN;
        break;
      case Token::KEYWORD_STATIC:
        storageKind = Symbol::STATIC;
        break;
      case Token::KEYWORD_AUTO:
        storageKind = Symbol::AUTO;
        break;
      case Token::KEYWORD_REGISTER:
        storageKind = Symbol::REGISTER;
        break;
      }
    } else {
      return error("Multiple storage specifiers not allowed");
    }
  }
  if (enableTypeQuali) {
    qualifier = Type::Qualifier(qConst, qVolatile, qRestrict);
  }
  CountedPtr<Type> type = nullptr;
  if (enableTypeSpec) {
    if (structType) {
      if (typeSpecs.size() != 0) {
        return error("Specified type and type specifier");
      }
      structType->setQualifier(qualifier);
      type = std::move(structType);
    } else {
      std::sort(typeSpecs.begin(), typeSpecs.end());
      auto it = typeSpecSets.find(typeSpecs);
      if (it == typeSpecSets.end()) {
        return error("Invalid type specifier set");
      }
      type = BasicType::create(it->second, qualifier);
    }
  }
  return DeclSpec(storageKind, std::move(type), qualifier);
}

void Parser::printErrCtx(std::ostream &os) {
  for (auto e : log.trace | std::views::reverse) {
    os << "... while parsing " << errCtxMsg(e) << "\n";
  }
}

const char *Parser::errCtxMsg(ErrCtx ctx) {
  switch (ctx) {
  case ErrCtx::EXPRESSION:
    return "expression";
  case ErrCtx::DECLARATOR:
    return "declarator";
  case ErrCtx::DECLARATION:
    return "declaration";
  case ErrCtx::DECL_SPEC:
    return "declaration specifiers";
  case ErrCtx::STRUCT:
    return "struct";
  case ErrCtx::FUNC_DEF:
    return "function definition";
  case ErrCtx::ST_IF:
    return "if statement";
  case ErrCtx::ST_FOR:
    return "for loop";
  case ErrCtx::ST_WHILE:
    return "while loop";
  case ErrCtx::ST_EXPRESSION:
    return "expression statement";
  }
  return "unknown";
}

ASTError Parser::errorExpectedToken(Token::Kind expectedKind, Token tok) {
  log.freeze();

  if (tok.isEmpty()) {
    tok = lex->peekToken();
  }

  std::cerr << "[Error][Parser] Expected " << Token::kindName(expectedKind)
            << " token, but got " << tok << '\n';

  return ASTError(ASTError::EXPECTED_TOKEN);
}
} // namespace c
