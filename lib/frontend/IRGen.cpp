#include "frontend/IRGen.h"
#include "frontend/AST.h"
#include "frontend/ASTVisitor.h"
#include "frontend/Semantics.h"
#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "ir/IR.h"
#include "ir/LazySSABuilder.h"
#include <cassert>
#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>

namespace {

class IRGenASTVisitor : public ASTVisitor<IRGenASTVisitor>,
                        public ExpressionSemantics::Handler {
public:
  IRGenASTVisitor() : sema(*this) {}

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
      for (auto &d : ast.declarators) {
        auto s =
            sym.declareSymbol(d.ident, Symbol(Symbol::AUTO, std::move(d.type)));
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
    sema.fromConstant(BasicType::create(Type::SINT));
  }

  void visitVar(VarAST &ast) {
    tmpSymbol = sym.getSymbol(ast.ident);
    if (!tmpSymbol) {
      error("Symbol undeclared");
    }
    sema.fromSymbol(*tmpSymbol);
  }

  void visitUnop(UnopAST &ast) { assert(false && "Unop unsupported"); }

  void visitBinop(BinopAST &ast) {
    switch (ast.getKind()) {
    case AST::ASSIGN: {
      dispatch(ast.getRHS());
      sema.expectRValue();
      Operand *rhs = tmpOperand;
      dispatch(ast.getLHS());
      sema.expectLValue();
      ssa.storeSSA(tmpSymbol->getId(), *rhs);
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::ADD: {
      dispatch(ast.getLHS());
      sema.expectRValue();
      Operand *lhs = tmpOperand;
      dispatch(ast.getRHS());
      sema.expectRValue();
      ssa->emitAdd(*lhs, *tmpOperand);
      tmpOperand = &ssa.getDef();
      sema.setCategory(ExpressionSemantics::RVALUE);
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

  ExpressionSemantics::Result semanticConvLValue() {
    tmpOperand = ssa.loadSSA(tmpSymbol->getId());
    if (!tmpOperand) {
      return ExpressionSemantics::ERROR;
    }
    return ExpressionSemantics::SUCCESS;
  }
  ExpressionSemantics::Result semanticSSADowngrade() {
    return ExpressionSemantics::ERROR;
  }
  void semanticError(ExpressionSemantics::Result err) {
    error("Semantics error");
  }

  Operand *tmpOperand = nullptr;
  Symbol *tmpSymbol = nullptr;

  SymbolTable sym;
  SSABuilder ssa;
  ExpressionSemantics sema;
};
} // namespace

std::unique_ptr<Program> IRGenAST(AST &ast) {
  auto gen = IRGenASTVisitor();
  gen.dispatch(ast);
  return gen.ssa.finish();
}
