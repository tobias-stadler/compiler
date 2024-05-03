#pragma once
#include "c/AST.h"
#include "c/Preprocessor.h"
#include "c/Symbol.h"
#include "c/Type.h"
#include "support/ErrorRecovery.h"
#include "support/RefCount.h"
#include <cassert>
#include <string_view>

namespace c {

class DeclSpec {
public:
  DeclSpec() = default;
  DeclSpec(Symbol::Kind symbolKind, Type *type,
           QualifiedType::Qualifier qualifier)
      : symbolKind(symbolKind), type(type), qualifier(qualifier) {}
  Symbol::Kind symbolKind = Symbol::EMPTY;
  Type *type;
  QualifiedType::Qualifier qualifier;
};

enum class Precedence : int {
  NONE = -1,
  COMMA,
  ASSIGN,
  TERNARY,
  LOG_OR,
  LOG_AND,
  BIT_OR,
  BIT_XOR,
  BIT_AND,
  EQUALITY,
  RELATIONAL,
  SHIFT,
  ADD,
  MUL,
};

class Parser {
public:
  Parser(ASTContext &ctx, PPLexer &lex, SymbolTable &sym)
      : lex(lex), sym(sym), ctx(ctx) {
    sym.clearScopeStack();
  }

  enum class ErrCtx {
    EXPRESSION,
    DECLARATOR,
    DECLARATION,
    DECL_SPEC,
    STRUCT,
    FUNC_DEF,
    FUNC_PARAMS,
    ST_EXPRESSION,
    ST_IF,
    ST_FOR,
    ST_WHILE,
  };

public:
  ASTPtrResult parseLiteralNum();

  ASTError error(std::string_view str = {"Unnamed error"});
  ASTError errorExpectedToken(Token::Kind expectedKind,
                              Token tok = Token::EMPTY);
  ASTError errorExpectedToken(std::string_view str, Token tok = Token::EMPTY);

  ASTError nop() { return ASTError(ASTError::NOP); };

  bool nextIsAbstractDeclarator();

  bool nextIsDeclarationList();

  ASTPtrResult parseStatement();

  ASTPtrResult parseExpression(Precedence prec = Precedence::NONE);

  ASTPtrResult parseInitializer();
  ASTPtrResult parseInitializerList();
  ASTResult<Type *> parseTypeName();
  ASTPtrResult parseUnary(bool enableCast = true);
  ASTPtrResult parsePostfix(AST::Ptr base);

  ASTPtrResult parseDeclaration();
  ASTResult<Type *> parseStruct();

  ASTResult<DeclaratorAST> parseDeclarator(bool abstract);
  ASTResult<DeclaratorAST> parseDirectDeclarator(bool abstract);
  ASTResult<DeclaratorAST> parseSingleDirectDeclarator(bool abstract,
                                                       bool first);

  ASTPtrResult parseBlockItem();
  ASTPtrResult parseCompoundStatement();
  ASTResult<std::unique_ptr<TranslationUnitAST>> parseTranslationUnit();

  ASTResult<DeclSpec> parseDeclSpec(bool enableType, bool enableQuali,
                                    bool enableStorage);

  void printErrCtx(std::ostream &os);

  static const char *errCtxMsg(ErrCtx ctx);

private:
  PPLexer &lex;
  SymbolTable &sym;
  FrameLogger<ErrCtx> log;
  ASTContext &ctx;
};
} // namespace c
