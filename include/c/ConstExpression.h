#pragma once

#include "c/AST.h"
#include "c/ASTVisitor.h"
#include "c/Semantics.h"
#include "support/MachineInt.h"
#include <cassert>

namespace c {

class ConstExpressionVisitor : public ASTVisitor<ConstExpressionVisitor>,
                               public ExpressionSemantics::Handler {
public:
  ConstExpressionVisitor(ASTContext &ctx, SymbolTable &sym)
      : ExpressionSemantics::Handler(ctx), sym(sym) {}

  class ExprState {
  public:
    ExprState(ExpressionSemantics sema, MInt val)
        : sema(std::move(sema)), val(std::move(val)) {}

    ExpressionSemantics sema;
    MInt val;
  };

  ASTResult<ExprState> state;

  void visit(AST &ast) {}

  void visitIntConst(IntConstAST &ast) {
    state = ExprState{ExpressionSemantics{*this}, ast.num};
  }

  void visitVar(VarAST &ast) {
  }

  void visitUnop(UnopAST &ast) {
    switch (ast.getKind()) {
    case AST::ADDR:
    case AST::DEREF:
    case AST::BIT_NOT:
    case AST::LOG_NOT:
    case AST::PLUS:
    case AST::MINUS:
    case AST::SIZEOF:
    case AST::ALIGNOF:
    default:
      visit(ast);
      return;
    }
  }

  void visitBinop(BinopAST &ast) {
    dispatch(ast.getLHS());
    auto lhs = state;
    dispatch(ast.getRHS());
    auto rhs = state;

    switch (ast.getKind()) {
    default:
      visit(ast);
      return;
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
      break;
    }
  }

  SymbolTable &sym;
};

} // namespace c
