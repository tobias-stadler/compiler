#pragma once

#include "c/AST.h"

#define VISIT_DELEGATE(AstTy)                                                  \
  impl().visit##AstTy(*static_cast<AstTy##AST *>(&ast));

namespace c {

template <class D> class ASTVisitor {

public:
  void dispatch(AST *ast) {
    if (!ast) {
      impl().visitNullptr();
      return;
    }
    dispatch(*ast);
  }

  void dispatch(AST &ast) {
    switch (ast.getKind()) {
    case AST::EMPTY:
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
    case AST::EQ:
    case AST::NEQ:
    case AST::LT:
    case AST::GT:
    case AST::LTE:
    case AST::GTE:
    case AST::COMMA:
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
    case AST::PLUS:
    case AST::MINUS:
      VISIT_DELEGATE(Unop);
      break;
    case AST::VAR:
      VISIT_DELEGATE(Var);
      break;
    case AST::CONST_INT:
      VISIT_DELEGATE(IntConst);
      break;
    case AST::ACCESS_MEMBER:
      VISIT_DELEGATE(MemberAccess);
      break;
    case AST::ACCESS_ARRAY:
      VISIT_DELEGATE(ArrAccess);
      break;
    case AST::FUNCTION_CALL:
      VISIT_DELEGATE(FunctionCall);
      break;
    case AST::ST_IF:
      VISIT_DELEGATE(IfSt);
      break;
    case AST::ST_COMPOUND:
      VISIT_DELEGATE(CompoundSt);
      break;
    case AST::ST_WHILE:
    case AST::ST_DO_WHILE:
      VISIT_DELEGATE(WhileSt);
      break;
    case AST::ST_FOR:
      VISIT_DELEGATE(ForSt);
      break;
    case AST::ST_RETURN:
      VISIT_DELEGATE(ReturnSt);
      break;
    case AST::ST_BREAK:
    case AST::ST_CONTINUE:
      VISIT_DELEGATE(LoopCtrlSt);
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
    case AST::CAST:
      VISIT_DELEGATE(Cast);
      break;
    case AST::ST_LABEL_NAMED:
    case AST::ST_LABEL_CASE:
    case AST::ST_LABEL_DEFAULT:
      VISIT_DELEGATE(LabelSt);
      break;
    case AST::ST_SWITCH:
      VISIT_DELEGATE(SwitchSt);
      break;
    case AST::ST_GOTO:
      VISIT_DELEGATE(GotoSt);
      break;
    case AST::TERNARY:
      VISIT_DELEGATE(Ternary);
      break;
    case AST::INITIALIZER_LIST:
      VISIT_DELEGATE(InitializerList);
      break;
    }
  }

protected:
  D &impl() { return static_cast<D &>(*this); }

  void visitNullptr() {}
  void visit(AST &ast) { impl().visit(ast); }
  void visitIntConst(IntConstAST &ast) { impl().visit(ast); }
  void visitVar(VarAST &ast) { impl().visit(ast); }
  void visitBinop(BinopAST &ast) { impl().visit(ast); }
  void visitUnop(UnopAST &ast) { impl().visit(ast); }
  void visitCompoundSt(CompoundStAST &ast) { impl().visit(ast); }
  void visitIfSt(IfStAST &ast) { impl().visit(ast); }
  void visitWhileSt(WhileStAST &ast) { impl().visit(ast); }
  void visitForSt(ForStAST &ast) { impl().visit(ast); }
  void visitLoopCtrlSt(LoopCtrlStAST &ast) { impl().visit(ast); }
  void visitReturnSt(ReturnStAST &ast) { impl().visit(ast); }
  void visitDeclarator(DeclaratorAST &ast) { impl().visit(ast); }
  void visitDeclaration(DeclarationAST &ast) { impl().visit(ast); }
  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    impl().visit(ast);
  }
  void visitTranslationUnit(TranslationUnitAST &ast) { impl().visit(ast); }
  void visitMemberAccess(MemberAccessAST &ast) { impl().visit(ast); }
  void visitArrAccess(ArrAccessAST &ast) { impl().visit(ast); }
  void visitFunctionCall(FunctionCallAST &ast) { impl().visit(ast); }
  void visitCast(CastAST &ast) { impl().visit(ast); }
  void visitLabelSt(LabelStAST &ast) { impl().visit(ast); }
  void visitGotoSt(GotoStAST &ast) { impl().visit(ast); }
  void visitSwitchSt(SwitchStAST &ast) { impl().visit(ast); }
  void visitTernary(TernaryAST &ast) { impl().visit(ast); }
  void visitInitializerList(InitializerListAST &ast) { impl().visit(ast); }
};

} // namespace c
