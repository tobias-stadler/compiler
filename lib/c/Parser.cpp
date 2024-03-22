#include "c/Parser.h"
#include "c/AST.h"
#include "c/Lexer.h"
#include "c/Symbol.h"
#include "c/Type.h"
#include "support/RefCount.h"
#include <algorithm>
#include <array>
#include <cassert>
#include <cerrno>
#include <charconv>
#include <cinttypes>
#include <format>
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
  if (!lex.matchNext(x)) [[unlikely]] {                                        \
    return errorExpectedToken(x);                                              \
  }

#define P_TOK_EXPECT_PEEK(x)                                                   \
  if (!lex.matchPeek(x)) [[unlikely]] {                                        \
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
  case Token::PUNCT_COMMA:
    return AST::COMMA;
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

constexpr Precedence binopPrecedence(AST::Kind kind) {
  switch (kind) {
  case AST::MUL:
  case AST::DIV:
    return Precedence::MUL;
  case AST::ADD:
  case AST::SUB:
    return Precedence::ADD;
  case AST::LSHIFT:
  case AST::RSHIFT:
    return Precedence::SHIFT;
  case AST::LT:
  case AST::GT:
  case AST::LTE:
  case AST::GTE:
    return Precedence::RELATIONAL;
  case AST::EQ:
  case AST::NEQ:
    return Precedence::EQUALITY;
  case AST::BIT_AND:
    return Precedence::BIT_AND;
  case AST::BIT_XOR:
    return Precedence::BIT_XOR;
  case AST::BIT_OR:
    return Precedence::BIT_OR;
  case AST::LOG_AND:
    return Precedence::LOG_AND;
  case AST::LOG_OR:
    return Precedence::LOG_OR;
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
    return Precedence::ASSIGN;
  case AST::COMMA:
    return Precedence::COMMA;
  default:
    return Precedence::NONE;
  }
}

constexpr bool precedenceRightAssoc(Precedence prec) {
  switch (prec) {
  default:
    return false;
  case Precedence::ASSIGN:
    return true;
  }
}

auto getTypeSpecSets() {
  std::vector<std::pair<std::vector<Token::Kind>, Type::Kind>> typeSpecSets{
      {{Token::KEYWORD_VOID}, Type::VOID},
      {{Token::KEYWORD_BOOL}, Type::BOOL},

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

ASTResult<Type *> Parser::parseTypeName() {
  auto tyRes = parseDeclSpec(true, true, false);
  P_AST_EXPECT_FWD(tyRes);
  auto decl = parseDeclarator(true);
  P_AST_EXPECT_OR_NOP(decl);
  if (!decl) {
    return tyRes->type;
  }
  if (!decl->ident.empty()) {
    return error("Illegal identifer in abstract declarator");
  }
  decl->spliceEnd(DeclaratorAST(tyRes->type, nullptr));
  return decl->type;
}

ASTPtrResult Parser::parseUnary(bool enableCast) {
  Token::Kind tokKind = lex.peekKind();
  AST::Ptr res;
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    lex.drop();
    auto tyRes = parseTypeName();
    P_AST_EXPECT_OR_NOP(tyRes);
    if (tyRes) {
      // Type cast disambiguation
      P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
      if (lex.matchPeek(Token::PUNCT_CURLYO)) {
        // Compound literal
        auto initRes = parseInitializerList();
        P_AST_EXPECT(initRes, "compound literal initializer");
        res = std::make_unique<CastAST>(initRes.moveRes(), tyRes.res());
        break;
      }
      if (!enableCast) {
        return std::make_unique<TypeAST>(tyRes.res());
      }
      auto subExpr = parseUnary();
      P_AST_EXPECT(subExpr, "unary expression after cast operator");
      return std::make_unique<CastAST>(subExpr.moveRes(), tyRes.res());
    }
    auto ex = parseExpression();
    P_AST_EXPECT(ex, "expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    res = ex.moveRes();
    break;
  }
  case Token::KEYWORD_SIZEOF:
  case Token::KEYWORD_ALIGNOF: {
    lex.drop();
    auto expr = parseUnary(false);
    P_AST_EXPECT(expr, "unary expression or type after sizeof/alignof");
    return std::make_unique<UnopAST>(
        tokKind == Token::KEYWORD_SIZEOF ? AST::SIZEOF : AST::ALIGNOF,
        expr.moveRes());
  }
  case Token::IDENTIFIER: {
    res = std::make_unique<VarAST>(std::string_view(lex.next()));
    break;
  }
  case Token::PP_NUM: {
    auto num = parseLiteralNum();
    P_AST_EXPECT(num, "number literal");
    res = num.moveRes();
    break;
  }
  default: {
    AST::Kind unopKind = tokenUnopKind(tokKind);
    if (unopKind == AST::EMPTY) {
      return nop();
    }
    lex.drop();
    auto subExpr = parseUnary();
    P_AST_EXPECT(subExpr, "unary expression after unary operator");
    if (unopKind == AST::ADDR && (*subExpr)->getKind() == AST::VAR) {
      auto *s = sym.getSymbol(static_cast<VarAST &>(*subExpr->get()).ident,
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

ASTPtrResult Parser::parseExpression(Precedence prec) {
  int precVal = static_cast<int>(prec);
  auto lhs = parseUnary();
  P_AST_EXPECT_FWD(lhs);
  while (true) {
    Token::Kind tokKind = lex.peekKind();
    if (tokKind == Token::PUNCT_QUESTION &&
        precVal <= static_cast<int>(Precedence::TERNARY)) {
      // Ternary
      lex.drop();
      auto ternaryLhs = parseExpression();
      P_AST_EXPECT(ternaryLhs, "lhs of ternary expression");
      P_TOK_EXPECT_NEXT(Token::PUNCT_COLON);
      auto ternaryRhs = parseExpression(Precedence::TERNARY);
      P_AST_EXPECT(ternaryRhs, "rhs of ternary expression");
      return std::make_unique<TernaryAST>(lhs, ternaryLhs, ternaryRhs);
    }
    AST::Kind binopKind = tokenBinopKind(tokKind);
    Precedence tokPrec = binopPrecedence(binopKind);
    if (binopKind == AST::EMPTY || static_cast<int>(tokPrec) < precVal) {
      return lhs;
    }
    lex.drop();
    auto rhs = parseExpression(
        precedenceRightAssoc(tokPrec)
            ? tokPrec
            : static_cast<Precedence>(static_cast<int>(tokPrec) + 1));
    P_AST_EXPECT(rhs, "expression");
    lhs = new BinopAST(binopKind, lhs, rhs);
  }
}

ASTPtrResult Parser::parseStatement() {
  Token::Kind tokKind = lex.peekKind();
  switch (tokKind) {
  case Token::PUNCT_SEMICOLON: {
    return error("Stray semicolon");
  }
  case Token::PUNCT_CURLYO: {
    return parseCompoundStatement();
  }
  case Token::KEYWORD_GOTO: {
    lex.drop();
    P_TOK_EXPECT_PEEK(Token::IDENTIFIER);
    std::string_view ident(lex.next());
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return std::make_unique<GotoStAST>(ident);
  }
  case Token::KEYWORD_CASE: {
    lex.drop();
    auto exprRes = parseExpression();
    P_AST_EXPECT(exprRes, "expression in case label");
    P_TOK_EXPECT_NEXT(Token::PUNCT_COLON);
    auto stRes = parseStatement();
    P_AST_EXPECT(stRes, "statement after case label");
    return std::make_unique<LabelStAST>(stRes.moveRes(), exprRes.moveRes());
  }
  case Token::KEYWORD_DEFAULT: {
    lex.drop();
    P_TOK_EXPECT_NEXT(Token::PUNCT_COLON);
    auto stRes = parseStatement();
    P_AST_EXPECT(stRes, "statement after default label");
    return std::make_unique<LabelStAST>(stRes.moveRes());
  }
  case Token::KEYWORD_IF: {
    P_ERR_CTX(ErrCtx::ST_IF);
    lex.drop();
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "condition expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    auto st = parseStatement();
    P_AST_EXPECT(st, "statement");
    if (lex.matchNext(Token::KEYWORD_ELSE)) {
      auto stElse = parseStatement();
      P_AST_EXPECT(stElse, "else statement");
      return new IfStAST(expr, st, stElse);
    }
    return new IfStAST(expr, st);
  }
  case Token::KEYWORD_WHILE: {
    P_ERR_CTX(ErrCtx::ST_WHILE);
    lex.drop();
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "condition expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    auto st = parseStatement();
    P_AST_EXPECT(st, "statement");
    return new WhileStAST(AST::ST_WHILE, expr, st);
  }
  case Token::KEYWORD_DO: {
    P_ERR_CTX(ErrCtx::ST_WHILE);
    lex.drop();
    auto st = parseStatement();
    P_AST_EXPECT(st, "statement");
    P_TOK_EXPECT_NEXT(Token::KEYWORD_WHILE);
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "condition expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return new WhileStAST(AST::ST_DO_WHILE, expr, st);
  }
  case Token::KEYWORD_FOR: {
    P_ERR_CTX(ErrCtx::ST_FOR);
    lex.drop();
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
    lex.drop();
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return std::make_unique<LoopCtrlStAST>(
        tokKind == Token::KEYWORD_CONTINUE ? AST::ST_CONTINUE : AST::ST_BREAK);
  }

  case Token::KEYWORD_RETURN: {
    lex.drop();
    auto exprRes = parseExpression();
    P_AST_EXPECT_OR_NOP(exprRes);
    AST::Ptr expr = nullptr;
    if (exprRes) {
      expr = exprRes.moveRes();
    }
    P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    return std::make_unique<ReturnStAST>(std::move(expr));
  }
  case Token::KEYWORD_SWITCH: {
    lex.drop();
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENO);
    auto exprRes = parseExpression();
    P_AST_EXPECT(exprRes, "expression for switch");
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    auto stRes = parseStatement();
    P_AST_EXPECT(stRes, "statement for switch");
    return std::make_unique<SwitchStAST>(exprRes.moveRes(), stRes.moveRes());
  }
  default: {
    P_ERR_CTX(ErrCtx::ST_EXPRESSION);
    auto exprRes = parseExpression();
    P_AST_EXPECT_FWD(exprRes);
    auto st = exprRes.moveRes();
    if (st->getKind() == AST::VAR && lex.matchNext(Token::PUNCT_COLON)) {
      // Label disambiguation
      auto labelName = static_cast<VarAST &>(*st).ident;
      auto stRes = parseStatement();
      P_AST_EXPECT(stRes, "statement after label");
      return std::make_unique<LabelStAST>(stRes.moveRes(), labelName);
    }
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
  P_TOK_EXPECT_PEEK(Token::PP_NUM);
  Token tok = lex.next();
  std::string str(tok);
  char *ptr = nullptr;
  errno = 0;
  auto num = std::strtoumax(str.c_str(), &ptr, 0);
  if (errno || ptr != str.c_str() + str.size()) {
    return errorExpectedToken("valid number literal", tok);
  }
  return new IntConstAST(MInt{32, num}, &ctx.make_type<BasicType>(Type::SINT));
}

ASTResult<DeclaratorAST> Parser::parseSingleDirectDeclarator(bool abstract,
                                                             bool first) {
  switch (lex.peekKind()) {
  case Token::PUNCT_PARENO: {
    lex.drop();
    if (abstract ? nextIsAbstractDeclarator() : first) {
      auto decl = parseDeclarator(abstract);
      P_AST_EXPECT(decl, "declarator");
      P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
      return decl;
    } else {
      P_ERR_CTX(ErrCtx::FUNC_PARAMS);
      auto *func = &ctx.make_type<FuncType>();
      if (!lex.matchPeek(Token::PUNCT_PARENC)) {
        do {
          auto spec = parseDeclSpec(true, true, false);
          P_AST_EXPECT(spec, "declaration specifiers");
          // TODO: correct handling of abstract declarators
          auto decl = parseDeclarator(true);
          P_AST_EXPECT_OR_NOP(decl);
          if (decl) {
            decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
            if (decl->ident.empty()) {
              func->addParam(*decl->type);
            } else {
              func->addNamedParam(decl->ident, *decl->type);
            }
          } else {
            func->addParam(*spec->type);
          }
        } while (lex.matchNext(Token::PUNCT_COMMA));
      }
      P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
      return DeclaratorAST(func);
    }
  }
  case Token::IDENTIFIER: {
    return DeclaratorAST(nullptr, nullptr, std::string_view(lex.next()));
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
  switch (lex.peekKind()) {
  case Token::PUNCT_STAR: {
    lex.drop();
    DerivedType *ty = &ctx.make_type<PtrType>(nullptr);
    auto spec = parseDeclSpec(false, true, false);
    P_AST_EXPECT_OR_NOP(spec);
    if (spec) {
      ty = &ctx.make_type<QualifiedType>(ty, spec->qualifier);
    }
    auto decl = parseDeclarator(abstract);
    if (abstract) {
      P_AST_EXPECT_OR_NOP(decl);
    } else {
      P_AST_EXPECT(decl, "declarator after pointer-declarator");
    }
    if (!decl) {
      return DeclaratorAST(ty);
    }
    decl->spliceEnd(DeclaratorAST(ty));
    return decl;
  }
  default:
    return parseDirectDeclarator(abstract);
  }
}

ASTPtrResult Parser::parseInitializerList() {
  P_TOK_EXPECT_NEXT(Token::PUNCT_CURLYO);
  auto res = std::make_unique<InitializerListAST>();
  do {
    if (lex.matchPeek(Token::PUNCT_CURLYC)) {
      break;
    }
    auto &entry = res->entries.emplace_back();
    if (lex.matchPeek(Token::PUNCT_SQUAREO) ||
        lex.matchPeek(Token::PUNCT_DOT)) {
      // FIXME: illegal, because [] postfix operator is converted to pointer add
      // TODO: parse manually
      auto designation = parsePostfix(nullptr);
      P_AST_EXPECT(designation, "designation");
      entry.designation = designation.moveRes();
      P_TOK_EXPECT_NEXT(Token::PUNCT_EQ);
    }
    auto initRes = parseInitializer();
    P_AST_EXPECT(initRes, "initializer");
    entry.initializer = initRes.moveRes();
  } while (lex.matchNext(Token::PUNCT_COMMA));
  P_TOK_EXPECT_NEXT(Token::PUNCT_CURLYC);
  return res;
}

ASTPtrResult Parser::parseInitializer() {
  if (lex.matchPeek(Token::PUNCT_CURLYO)) {
    return parseInitializerList();
  }
  return parseExpression(Precedence::ASSIGN);
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
    if (!sym.scope().isFile()) {
      return error("Function definition only allowed in file scope");
    }
    if (decl->type->getKind() != Type::FUNC) {
      return error("Function declarator must declare function");
    }

    auto ast = std::make_unique<FunctionDefinitionAST>(spec->symbolKind,
                                                       decl.moveRes());
    Symbol::Kind symbolKind = sym.scope().getSymbolKind(ast->symbolKind);

    if (!(symbolKind == Symbol::EXTERN || symbolKind == Symbol::STATIC)) {
      return error("Invalid storage class for function");
    }
    auto *s = sym.declareSymbol(Symbol(symbolKind, ast->decl.type, decl->ident,
                                       Symbol::Namespace::ORDINARY));
    // TODO: prototype support
    if (!s) {
      return error("Function redefined");
    }

    sym.pushScope(ast->funcScope);
    sym.pushScope(ast->blockScope);
    for (auto [ident, ty] : ast->getType().getParams()) {
      sym.declareSymbol(
          Symbol(Symbol::AUTO, ty, ident, Symbol::Namespace::ORDINARY));
    }
    auto st = parseCompoundStatement();
    P_AST_EXPECT(st, "compound statement for function definition");
    ast->st = st.moveRes();
    sym.popScope();
    sym.popScope();

    return ast;
  }

  auto ast = std::make_unique<DeclarationAST>(spec->symbolKind);
  while (true) {
    AST::Ptr initializer = nullptr;
    if (lex.matchNext(Token::PUNCT_EQ)) {
      auto initRes = parseInitializer();
      P_AST_EXPECT(initRes, "initializer");
      initializer = initRes.moveRes();
    }
    ast->declarators.emplace_back(decl.moveRes(), std::move(initializer));
    if (!lex.matchNext(Token::PUNCT_COMMA)) {
      break;
    }
    decl = parseDeclarator(false);
    P_AST_EXPECT(decl, "declarator");
    decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
  }

  P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);

  Symbol::Kind symbolKind = sym.scope().getSymbolKind(ast->symbolKind);
  // TODO: verify symbolKind

  for (auto &[d, _] : ast->declarators) {
    auto *s = sym.declareSymbol(
        Symbol(symbolKind, d.type, d.ident, Symbol::Namespace::ORDINARY));
    if (!s) {
      return error(std::string("Symbol redeclared: ") + std::string(d.ident));
    }
  }
  return ast;
}

ASTResult<std::unique_ptr<TranslationUnitAST>> Parser::parseTranslationUnit() {
  auto tu = std::make_unique<TranslationUnitAST>();
  sym.pushScope(tu->scope);
  while (true) {
    auto declRes = parseDeclaration();
    P_AST_EXPECT_OR_NOP(declRes);
    if (!declRes) {
      break;
    }
    tu->declarations.push_back(declRes.moveRes());
  }
  sym.popScope();
  return tu;
}

ASTPtrResult Parser::parseCompoundStatement() {
  if (lex.peekKind() != Token::PUNCT_CURLYO) {
    return nop();
  }
  lex.drop();
  auto st = std::make_unique<CompoundStAST>();
  sym.pushScope(st->scope);
  while (true) {
    ASTPtrResult subSt = parseBlockItem();
    P_AST_EXPECT_OR_NOP(subSt);
    if (!subSt) {
      break;
    }
    st->children.push_back(subSt);
  }
  P_TOK_EXPECT_NEXT(Token::PUNCT_CURLYC);
  sym.popScope();
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
  switch (lex.peekKind()) {
  case Token::PUNCT_STAR:
  case Token::PUNCT_PARENO:
  case Token::PUNCT_SQUAREO:
    return true;
  default:
    return false;
  }
}

bool Parser::nextIsDeclarationList() {
  switch (lex.peekKind()) {
  case Token::PUNCT_EQ:
  case Token::PUNCT_COMMA:
  case Token::PUNCT_SEMICOLON:
    return false;
  default:
    return true;
  }
}

ASTResult<Type *> Parser::parseStruct() {
  P_ERR_CTX(ErrCtx::STRUCT);
  bool isUnion = false;
  bool isStruct = false;
  bool isEnum = false;
  switch (lex.peekKind()) {
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
  lex.drop();
  std::string_view name;
  Type *res = nullptr;
  if (lex.matchPeek(Token::IDENTIFIER)) {
    name = std::string_view(lex.next());
    if (auto *s = sym.getSymbol(name, Symbol::Namespace::TAG)) {
      res = &s->getType();
      if (!((isUnion && res->getKind() == Type::UNION) ||
            (isStruct && res->getKind() == Type::STRUCT) ||
            (isEnum && res->getKind() == Type::ENUM))) {
        return error("Redeclared tag with different type");
      }
    }
  }
  if (!res) {
    if (isEnum) {
      res = &ctx.make_type<EnumType>(&ctx.make_type<BasicType>(Type::SINT));
    } else {
      res = &ctx.make_type<StructType>(isUnion);
    }
    if (!name.empty()) {
      sym.declareSymbol(
          Symbol(Symbol::TYPEDEF, res, name, Symbol::Namespace::TAG));
    }
  }
  if (!lex.matchNext(Token::PUNCT_CURLYO)) {
    if (name.empty()) {
      return error("Expected identifier or struct declaration list");
    }
    return res;
  }
  if (res->isComplete()) {
    return error("Tagged type is already complete");
  }

  if (isEnum) {
    auto *enumConstTy = &ctx.make_type<BasicType>(Type::SINT);
    auto &enumTy = as<EnumType>(*res);
    std::string_view oldValName;
    do {
      if (!lex.matchPeek(Token::IDENTIFIER)) {
        break;
      }
      std::string_view valName = lex.next();
      AST::Ptr init = nullptr;
      if (lex.matchNext(Token::PUNCT_EQ)) {
        auto expr = parseExpression(Precedence::TERNARY);
        P_AST_EXPECT(expr, "initializer expression");
        init = expr.moveRes();
      } else {
        if (oldValName.empty()) {
          init = std::make_unique<IntConstAST>(MInt::zero(32), enumConstTy);
        } else {
          init = std::make_unique<BinopAST>(
              AST::ADD, std::make_unique<VarAST>(oldValName),
              std::make_unique<IntConstAST>(MInt::one(32), enumConstTy));
        }
      }
      auto *s = sym.declareSymbol(Symbol(std::move(init), enumConstTy, valName,
                                         Symbol::Namespace::ORDINARY));
      if (!s) {
        return error("Enum member identifer redeclares another symbol");
      }
      enumTy.members.push_back(s);
      oldValName = valName;
    } while (lex.matchNext(Token::PUNCT_COMMA));
  } else {
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
        if (!decl->type->isComplete()) {
          return error("Illegal incomplete type");
        }
        as<StructType>(*res).addNamedMember(decl->ident, *decl->type);
      } while (lex.matchNext(Token::PUNCT_COMMA));
      P_TOK_EXPECT_NEXT(Token::PUNCT_SEMICOLON);
    }
  }

  res->setComplete(true);
  P_TOK_EXPECT_NEXT(Token::PUNCT_CURLYC);
  return res;
}

ASTPtrResult Parser::parsePostfix(AST::Ptr base) {
  AST::Ptr res;
  Token::Kind tokKind = lex.peekKind();
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    lex.drop();
    auto call = std::make_unique<FunctionCallAST>(std::move(base));
    if (!lex.matchPeek(Token::PUNCT_PARENC)) {
      do {
        auto expr = parseExpression(Precedence::ASSIGN);
        P_AST_EXPECT(expr, "expression");
        call->args.push_back(expr.moveRes());
      } while (lex.matchNext(Token::PUNCT_COMMA));
    }
    P_TOK_EXPECT_NEXT(Token::PUNCT_PARENC);
    res = std::move(call);
    break;
  }
  case Token::PUNCT_SQUAREO: {
    lex.drop();
    auto expr = parseExpression();
    P_AST_EXPECT(expr, "expression");
    P_TOK_EXPECT_NEXT(Token::PUNCT_SQUAREC);
    res = std::make_unique<UnopAST>(
        AST::DEREF,
        std::make_unique<BinopAST>(AST::ADD, std::move(base), expr.moveRes()));
    break;
  }
  case Token::PUNCT_DOT:
  case Token::PUNCT_ARROW: {
    lex.drop();
    P_TOK_EXPECT_PEEK(Token::IDENTIFIER);
    if (tokKind == Token::PUNCT_ARROW) {
      base = std::make_unique<UnopAST>(AST::DEREF, std::move(base));
    }
    res = std::make_unique<MemberAccessAST>(std::move(base), lex.next());
    break;
  }
  case Token::PUNCT_PLUSPLUS:
  case Token::PUNCT_MINUSMINUS: {
    lex.drop();
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
  Symbol::Kind storageKind = Symbol::EMPTY;
  Type *ty = nullptr;
  std::vector<Token::Kind> typeSpecs;
  int qConst = 0;
  int qVolatile = 0;
  int qRestrict = 0;
  std::vector<Token::Kind> storageSpecs;
  while (true) {
    Token::Kind kind = lex.peekKind();
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
    case Token::KEYWORD_BOOL:
      if (enableTypeSpec) {
        typeSpecs.push_back(kind);
        lex.drop();
        break;
      } else {
        goto done;
      }
    case Token::KEYWORD_STRUCT:
    case Token::KEYWORD_UNION:
    case Token::KEYWORD_ENUM:
      if (enableTypeSpec) {
        if (ty) {
          return error("Specified multiple types");
        }
        auto tyRes = parseStruct();
        P_AST_EXPECT(tyRes, "struct/union/enum");
        ty = *tyRes;
        break;
      } else {
        goto done;
      }
    case Token::IDENTIFIER:
      if (enableTypeSpec && !ty) {
        auto *s = sym.getSymbol(lex.peek(), Symbol::Namespace::ORDINARY);
        if (!s || s->getKind() != Symbol::TYPEDEF) {
          goto done;
        }
        lex.drop();
        ty = &s->getType();
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
        lex.drop();
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
        lex.drop();
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

  if (enableTypeSpec) {
    if (ty) {
      if (typeSpecs.size() != 0) {
        return error("Specified type and type specifier");
      }
    } else {
      std::sort(typeSpecs.begin(), typeSpecs.end());
      auto it = typeSpecSets.find(typeSpecs);
      if (it == typeSpecSets.end()) {
        return error("Invalid type specifier set");
      }
      ty = &ctx.make_type<BasicType>(it->second);
    }
  }

  QualifiedType::Qualifier qualifier = QualifiedType::Qualifier();
  if (enableTypeQuali) {
    qualifier = QualifiedType::Qualifier(qConst, qVolatile, qRestrict);
    if (ty) {
      ty = &ctx.qualifyType(*ty, qualifier);
    }
  }
  return DeclSpec(storageKind, ty, qualifier);
}

void Parser::printErrCtx(std::ostream &os) {
  for (auto e : log.trace | std::views::reverse) {
    os << "... while parsing " << errCtxMsg(e) << "\n";
  }
}

const char *Parser::errCtxMsg(ErrCtx errCtx) {
  switch (errCtx) {
  case ErrCtx::EXPRESSION:
    return "expression";
  case ErrCtx::DECLARATOR:
    return "declarator";
  case ErrCtx::DECLARATION:
    return "declaration";
  case ErrCtx::DECL_SPEC:
    return "declaration specifiers";
  case ErrCtx::STRUCT:
    return "struct/union/enum";
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
  case ErrCtx::FUNC_PARAMS:
    return "function parameters";
  }
  return "unknown";
}

ASTError Parser::errorExpectedToken(std::string_view str, Token tok) {
  log.freeze();

  if (tok.isEmpty()) {
    tok = lex.peek();
  }

  std::cerr << "[Error][Parser] Expected " << str << ", but got " << tok
            << '\n';

  return ASTError(ASTError::EXPECTED_TOKEN);
}

ASTError Parser::errorExpectedToken(Token::Kind expectedKind, Token tok) {
  return errorExpectedToken(std::format("'{}'", Token::kindName(expectedKind)),
                            tok);
}
} // namespace c
