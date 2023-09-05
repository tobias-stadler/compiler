#pragma once

#include "frontend/AST.h"

#define VISIT_DELEGATE(AstTy)                                                  \
  impl().visit##AstTy(*static_cast<AstTy##AST *>(&ast));

template <class D> class ASTVisitor {

public:
  void dispatch(AST *ast) {
    if (!ast) {
      return;
    }
    dispatch(*ast);
  }

  void dispatch(AST &ast) {
    switch (ast.getKind()) {
    case AST::EMPTY:
      break;
    default:
      impl().visit(ast);
      break;
    case AST::ASSIGN:
    case AST::ASSIGN_ADD:
    case AST::ASSIGN_SUB:
    case AST::ASSIGN_MUL:
    case AST::ASSIGN_DIV:
    case AST::ASSIGN_MOD:
    case AST::ASSIGN_AND:
    case AST::ASSIGN_OR:
    case AST::ASSIGN_XOR:
    case AST::ASSIGN_LSHIFT:
    case AST::ASSIGN_RSHIFT:
    case AST::ADD:
    case AST::SUB:
    case AST::MUL:
    case AST::DIV:
    case AST::MOD:
    case AST::BIT_AND:
    case AST::BIT_OR:
    case AST::BIT_XOR:
    case AST::LSHIFT:
    case AST::RSHIFT:
    case AST::LOG_AND:
    case AST::LOG_OR:
      VISIT_DELEGATE(Binop);
      break;
    case AST::INC_PRE:
    case AST::INC_POST:
    case AST::DEC_PRE:
    case AST::DEC_POST:
    case AST::ADDR:
    case AST::DEREF:
    case AST::BIT_NOT:
    case AST::LOG_NOT:
      VISIT_DELEGATE(Unop);
      break;
    case AST::VAR:
      VISIT_DELEGATE(Var);
      break;
    case AST::NUM:
      VISIT_DELEGATE(Num);
      break;
    case AST::ST_IF:
      VISIT_DELEGATE(IfSt);
      break;
    case AST::ST_COMPOUND:
      VISIT_DELEGATE(CompoundSt);
      break;
    case AST::ST_WHILE:
      VISIT_DELEGATE(WhileSt);
      break;
    case AST::DECLARATOR:
      VISIT_DELEGATE(Declarator);
      break;
    case AST::DECLARATION:
      VISIT_DELEGATE(Declaration);
      break;
    case AST::FUNCTION_DEFINITION:
      VISIT_DELEGATE(FunctionDefinition);
      break;
    case AST::TRANSLATION_UNIT:
      VISIT_DELEGATE(TranslationUnit);
      break;
    }
  }

protected:
  D &impl() { return static_cast<D &>(*this); }

  void visit(AST &) {}
  void visitNum(NumAST &) {}
  void visitVar(VarAST &) {}
  void visitBinop(BinopAST &) {}
  void visitUnop(UnopAST &) {}
  void visitCompoundSt(CompoundStAST &) {}
  void visitIfSt(IfStAST &) {}
  void visitWhileSt(WhileStAST &) {}
  void visitDeclarator(DeclaratorAST &) {}
  void visitDeclaration(DeclarationAST &) {}
  void visitFunctionDefinition(FunctionDefinitionAST &) {}
  void visitTranslationUnit(TranslationUnitAST &) {}
};
