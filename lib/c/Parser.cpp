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

static auto typeSpecSets = getTypeSpecSets();
} // namespace

ASTPtrResult Parser::parseUnary() {
  Token::Kind tokKind = lex->peekTokenKind();
  AST::Ptr res;
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    lex->dropToken();
    auto ex = parseExpression();
    if (!ex) {
      return error("Expected expresssion");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      return error("Expected closing paren");
    }
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
    if (!subExpr) {
      return error("Expected expression after unary operator");
    }
    if (unopKind == AST::ADDR && (*subExpr)->getKind() == AST::VAR) {
      auto *s = sym->getSymbol(static_cast<VarAST &>(*subExpr->get()).ident);
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
  Token::Kind tokKind = lex->peekTokenKind();
  switch (tokKind) {
  case Token::PUNCT_SEMICOLON: {
    return error("Stray semicolon");
  }
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
      return error("Expected opening paren in while loop");
    }
    auto expr = parseExpression();
    if (!expr) {
      return error("Expected expression in while loop");
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
  case Token::KEYWORD_FOR: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
      return error("Expected opening paren after for keyword");
    }
    auto initClauseRes = parseDeclaration();
    AST::Ptr initClause = nullptr;
    if (initClauseRes.isNop()) {
      initClauseRes = parseExpression();
      if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
        return error("Expected semicolon after init expression in for loop");
      }
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
    if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
      return error("Expected semicolon after condition expression in for loop");
    }
    auto exprIterRes = parseExpression();
    AST::Ptr exprIter = nullptr;
    if (exprIterRes) {
      exprIter = exprIterRes.moveRes();
    } else if (!exprIterRes.isNop()) {
      return error("Expected iter expression in for loop");
    }
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      return error("Expected closing paren after for header");
    }
    auto stRes = parseStatement();
    if (!stRes) {
      return error("Expected statment after for");
    }
    return std::make_unique<ForStAST>(std::move(initClause),
                                      std::move(exprCond), std::move(exprIter),
                                      stRes.moveRes());
  }
  case Token::KEYWORD_CONTINUE:
  case Token::KEYWORD_BREAK: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
      return error("Expected semicolon after loop control statement");
    }
    return std::make_unique<LoopCtrlStAST>(
        tokKind == Token::KEYWORD_CONTINUE ? AST::ST_CONTINUE : AST::ST_BREAK);
  }

  case Token::KEYWORD_RETURN: {
    lex->dropToken();
    auto exprRes = parseExpression();
    AST::Ptr expr = nullptr;
    if (exprRes) {
      expr = exprRes.moveRes();
    } else if (!exprRes.isNop()) {
      return error("Expected expression in return statement");
    }
    if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
      return error("Expected semicolon after return statement");
    }
    return std::make_unique<ReturnStAST>(std::move(expr));
  }
  default: {
    auto st = parseExpression();
    if (st.isNop()) {
      return nop();
    } else if (!st) {
      return error("Expected expresssion statement");
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
          auto spec = parseDeclSpec(true, true, false);
          if (!spec) {
            return error("Expected decl specifiers");
          }
          auto decl = parseDeclarator(false);
          if (!decl) {
            return error("Expected declarator");
          }
          decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
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
    auto spec = parseDeclSpec(false, true, false);
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
  auto spec = parseDeclSpec(true, true, true);
  if (spec.isNop()) {
    return nop();
  }
  if (!spec) {
    return error("Expected declaration specifiers");
  }

  auto decl = parseDeclarator(false);
  if (!decl) {
    return error("Expected valid declarator");
  }
  decl->spliceEnd(DeclaratorAST(spec->type, nullptr));

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
  while (true) {
    AST::Ptr initializer = nullptr;
    if (lex->matchNextToken(Token::PUNCT_EQ)) {
      auto expr = parseExpression();
      if (!expr) {
        return error("Expected valid expression as initializer");
      }
      initializer = expr.moveRes();
    }
    ast->declarators.emplace_back(decl.moveRes(), std::move(initializer));
    if (!lex->matchNextToken(Token::PUNCT_COMMA)) {
      break;
    }
    decl = parseDeclarator(false);
    if (!decl) {
      return error("Expected valid declarator");
    }
    decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
  }

  if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
    return error("Expected semicolon after declaration");
  }

  if (sym->scope().isFile()) {
    // TODO: global variable declaration in symbol table
    assert(false && "global variable declaration unsupported");
  } else {
    for (auto &[d, _] : ast->declarators) {
      // TOOD: proper handling of storage spec
      auto *s = sym->declareSymbol(d.ident, Symbol(Symbol::AUTO, d.type));
      if (!s) {
        return error(std::string("Symbol redeclared: ") + std::string(d.ident));
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
  while (true) {
    ASTPtrResult subSt = parseBlockItem();
    if (!subSt) {
      if (subSt.isNop()) {
        break;
      }
      return error("Expected block item");
    }
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

ASTResult<CountedPtr<StructType>> Parser::parseStruct() {
  bool isUnion = false;
  switch (lex->peekTokenKind()) {
  case Token::KEYWORD_UNION:
    isUnion = true;
  case Token::KEYWORD_STRUCT:
    break;
  default:
    return nop();
  }
  lex->dropToken();
  std::string_view name;
  if (lex->matchPeekToken(Token::IDENTIFIER)) {
    name = lex->nextToken();
  }
  auto res = make_counted<StructType>(isUnion, name);
  if (!lex->matchNextToken(Token::PUNCT_CURLYO)) {
    if (name.empty()) {
      return error("Expected identifier or struct declaration list");
    }
    return res;
  }

  while (true) {
    auto spec = parseDeclSpec(true, true, false);
    if (spec.isNop()) {
      break;
    } else if (!spec) {
      return error("Expected declaration specifiers");
    }
    do {
      auto decl = parseDeclarator(false);
      if (!decl) {
        return error("Expected declarator");
      }
      decl->spliceEnd(DeclaratorAST(spec->type, nullptr));
      res->addNamedMember(decl->ident, std::move(decl->type));
    } while (lex->matchNextToken(Token::PUNCT_COMMA));
    if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
      return error("Expected semicolon after member declaration");
    }
  }

  if (!lex->matchNextToken(Token::PUNCT_CURLYC)) {
    return error("Expected closing curly");
  }
  return res;
}

ASTPtrResult Parser::parsePostfix(AST::Ptr base) {
  AST::Ptr res;
  Token::Kind tokKind = lex->peekTokenKind();
  switch (tokKind) {
  case Token::PUNCT_PARENO: {
    lex->dropToken();
    if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
      return error("Expected closing paren for function call");
    }
    break;
  }
  case Token::PUNCT_SQUAREO: {
    lex->dropToken();
    auto expr = parseExpression();
    if (!expr) {
      return error("Expected expression for array access");
    }
    if (!lex->matchNextToken(Token::PUNCT_SQUAREC)) {
      return error("Expected closing bracket for array access");
    }
    res = std::make_unique<ArrAccessAST>(std::move(base), expr.moveRes());
    break;
  }
  case Token::PUNCT_DOT:
  case Token::PUNCT_ARROW: {
    lex->dropToken();
    if (!lex->matchPeekToken(Token::IDENTIFIER)) {
      return error("Expected identifer for member access");
    }
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
  bool isNop = true;
  Type::Qualifier qualifier = Type::Qualifier();
  Symbol::Kind storageKind = Symbol::EMPTY;
  CountedPtr<Type> type;
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
        if (type) {
          return error("Specified multiple types");
        }
        auto tyRes = parseStruct();
        if (!tyRes) {
          return error("Expected struct/union");
        }
        type = tyRes.moveRes();
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
        storageKind = Symbol::TYPE;
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
  if (enableTypeSpec) {
    if (type) {
      if (typeSpecs.size() != 0) {
        return error("Specified type and type specifier");
      }
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

} // namespace c
