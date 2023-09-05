#include "frontend/IRGen.h"
#include "frontend/AST.h"
#include "frontend/ASTVisitor.h"
#include "frontend/Semantics.h"
#include "ir/ConstructSSALazy.h"
#include "ir/IR.h"
#include <cassert>
#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>

namespace {

class IRGenASTVisitor : public ASTVisitor<IRGenASTVisitor> {
public:
  void visitTranslationUnit(TranslationUnitAST &ast) {
    sym.pushScope(Scope::FILE);
    for (auto &d : ast.declarations) {
      dispatch(d.get());
    }
    sym.popScope();
  }

  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    ssa.startFunction();
    ssa.startBlock();
    sym.declareSymbol(ast.decl.ident,
                      Symbol(Symbol::EXTERN, std::move(ast.decl.type)));
    sym.pushScope(Scope::FUNC);
    sym.pushScope(Scope::BLOCK);
    /*
    for (auto [name, idx] : ast.getType().getParamIndex()) {
      sym.declareSymbol(name,
                        Symbol(Symbol::AUTO, ast.getType().getParamType(idx)));
    }
    */
    for (auto &child : ast.getStatement().children) {
      dispatch(child.get());
    }
    sym.popScope();
    sym.popScope();
    ssa.endBlock();
    ssa.endFunction();
  }

  void visitDeclaration(DeclarationAST &ast) {
    if (sym.scope().isFile()) {
      assert(false && "Global declaration unsupported");
    } else if (sym.scope().isBlock()) {
      Symbol::Kind k = ast.spec.symbolKind;
      k = k == Symbol::EMPTY ? Symbol::AUTO : k;
      for (auto &d : ast.declarators) {
        auto s = sym.declareSymbol(d.ident, Symbol(k, std::move(d.type)));
        if (!s) {
          error("Symbol redeclared");
        }
      }
    }
  }

  void visitCompoundSt(CompoundStAST &ast) {
    sym.pushScope(Scope::BLOCK);
    for (auto &child : ast.children) {
      dispatch(child.get());
    }
    sym.popScope();
  }

  void visit(AST &) {}

  void visitNum(NumAST &ast) {
    ssa->emitConstInt(IntSSAType::get(32), ast.num);
    tmpOperand = &ssa.getDef();
  }

  void visitVar(VarAST &ast) {
    tmpSymbol = sym.getSymbol(ast.ident);
    if (!tmpSymbol) {
      error("Symbol undeclared");
    }
    tmpOperand = ssa.loadLocal(tmpSymbol->getId());
    if (!tmpOperand) {
      error("Def not found");
    }
  }

  void visitBinop(BinopAST &ast) {
    switch (ast.getKind()) {
    case AST::ASSIGN: {
      assert(ast.getLHS().getKind() == AST::VAR);
      VarAST &lhs = static_cast<VarAST &>(ast.getLHS());
      Symbol *s = sym.getSymbol(lhs.ident);
      if (!s) {
        error("Symbol undeclared");
      }
      dispatch(ast.getRHS());
      ssa.storeLocal(s->getId(), *tmpOperand);
      break;
    }
    case AST::ADD: {
      dispatch(ast.getLHS());
      Operand *lhs = tmpOperand;
      dispatch(ast.getRHS());
      ssa->emitAdd(*lhs, *tmpOperand);
      tmpOperand = &ssa.getDef();
      break;
    }
    default:
      assert(false && "unsupported");
      break;
    }
  }

  void visitIfSt(IfStAST &ast) {
    Block &src = ssa.endBlock();

    ssa.startBlock();
    Block &dstTrue = ssa.endBlock();

    dispatch(ast.getStatement());

    if (ast.hasElseStatement()) {
      ssa.startBlock();
      dispatch(ast.getElseStatement());
      Block &dstFalse = ssa.endBlock();
    }

    ssa.startBlock();
  }
  void visitWhileSt(WhileStAST &ast) {}
  void visitDeclarator(DeclaratorAST &ast) {}

  void error(std::string_view err) {
    std::cerr << "[IRGen] " << err << "\n";
    exit(1);
  }

  Operand *tmpOperand;
  Symbol *tmpSymbol;
  SymbolTable sym;
  SSABuilder ssa;
};
} // namespace

std::unique_ptr<Program> IRGenAST(AST &ast) {
  auto gen = IRGenASTVisitor();
  gen.dispatch(ast);
  return gen.ssa.finish();
}
