#include "frontend/IRGen.h"
#include "frontend/AST.h"
#include "frontend/ASTVisitor.h"
#include "frontend/Semantics.h"
#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "ir/IR.h"
#include "ir/IRBuilder.h"
#include "ir/InstrBuilder.h"
#include "ir/LazySSABuilder.h"
#include <cassert>
#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

namespace {

class StorageBuilder {

public:
  StorageBuilder(IRBuilder &ir) : ir(ir) {}
  enum StorageKind {
    EMPTY,
    SSA,
    STACK,
  };

  class StorageInfo {
  public:
    StorageInfo(Symbol &symbol) : symbol(&symbol) {}
    Symbol *symbol;
    StorageKind kind = EMPTY;
    Operand *ptr = nullptr;
    std::vector<Operand *> ssaDefs;
  };

  static SSAType &memType(Type &type) {
    SSAType *ssaType;
    switch (type.getKind()) {
    case Type::SCHAR:
    case Type::UCHAR:
      ssaType = &IntSSAType::get(8);
      break;
    case Type::SSHORT:
    case Type::USHORT:
      ssaType = &IntSSAType::get(16);
      break;
    case Type::SINT:
    case Type::UINT:
      ssaType = &IntSSAType::get(32);
      break;
    case Type::PTR:
      ssaType = &PtrSSAType::get();
      break;
    default:
      assert(false && "Unsupported memory type");
      break;
    }
    return *ssaType;
  }

  StorageInfo &declareLocal(Symbol &s) {
    auto [it, succ] = slots.emplace(s.getId(), s);
    return it->second;
  }

  void allocateStack(StorageInfo &s) {
    assert(s.kind == EMPTY);
    SSAType &ssaType = memType(*s.symbol->getType());
    Operand &ptr = ir->emitAlloca(ssaType, 1).getDef();
    s.kind = STACK;
    s.ptr = &ptr;
  }

  void allocateSSA(StorageInfo &s) {
    assert(s.kind == EMPTY);
    s.kind = SSA;
    s.ptr = nullptr;
  }

  void setFunction(Function &func) {
    slots.clear();
    currFunc = &func;
  }

  void trackSSADef(StorageInfo &s, Operand &operand) {
    assert(s.kind == SSA);
    s.ssaDefs.push_back(&operand);
  }

  StorageInfo &get(SSASymbolId id) {
    auto it = slots.find(id);
    assert(it != slots.end());
    return it->second;
  }

private:
  Function *currFunc;
  IRBuilder &ir;
  std::unordered_map<SSASymbolId, StorageInfo> slots;
};

class IRGenASTVisitor : public ASTVisitor<IRGenASTVisitor>,
                        public ExpressionSemantics::Handler {
public:
  IRGenASTVisitor(SymbolTable &sym) : sym(sym), sema(*this), storage(ir) {}

  void visitTranslationUnit(TranslationUnitAST &ast) {
    sym.pushScope(ast.scope);
    for (auto &d : ast.declarations) {
      dispatch(d.get());
    }
    sym.popScope();
  }

  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    ir.startFunction();
    storage.setFunction(ir.getFunc());
    ir.startBlock();
    sym.pushScope(ast.funcScope);
    sym.pushScope(ast.blockScope);
    /*
    for (auto [name, idx] : ast.getType().getParamIndex()) {
      sym.declareSymbol(name,
                        Symbol(Symbol::AUTO, ast.getType().getParamType(idx)));
    }
    */
    dispatch(ast.getStatement());
    sym.popScope();
    sym.popScope();
    ir.endBlock();
    SSARenumberTable::destroy(ir.getFunc());
    ir.endFunction();
  }

  void visitDeclaration(DeclarationAST &ast) {
    if (sym.scope().isFile()) {
      assert(false && "Global declaration unsupported");
    } else if (sym.scope().isBlock()) {
      for (auto &d : ast.declarators) {
        auto *s = sym.getSymbol(d.ident);
        assert(s && "Symbol undeclared");
        auto &sInfo = storage.declareLocal(*s);
        if (s->isAddrTaken()) {
          storage.allocateStack(sInfo);
        } else {
          storage.allocateSSA(sInfo);
        }
      }
    }
  }

  void visitCompoundSt(CompoundStAST &ast) {
    sym.pushScope(ast.scope);
    for (auto &child : ast.children) {
      dispatch(child.get());
    }
    sym.popScope();
  }

  void visit(AST &) {}

  void visitNum(NumAST &ast) {
    ir->emitConstInt(IntSSAType::get(32), ast.num);
    tmpOperand = &ir.getDef();
    sema.fromConstant(BasicType::create(Type::SINT));
  }

  void visitVar(VarAST &ast) {
    tmpSymbol = sym.getSymbol(ast.ident);
    if (!tmpSymbol) {
      error("Symbol undeclared");
    }
    sema.fromSymbol(*tmpSymbol);
    auto &s = storage.get(tmpSymbol->getId());
    tmpOperand = s.ptr;
  }

  void visitUnop(UnopAST &ast) {
    switch (ast.getKind()) {
    case AST::ADDR: {
      dispatch(ast.getSubExpression());
      sema.addr();
      break;
    }
    case AST::DEREF: {
      dispatch(ast.getSubExpression());
      sema.deref();
      break;
    }
    default:
      assert(false && "unsupported");
      break;
    }
  }

  void visitBinop(BinopAST &ast) {
    switch (ast.getKind()) {
    case AST::ASSIGN: {
      dispatch(ast.getRHS());
      sema.expectRValue();
      Operand *rhs = tmpOperand;
      dispatch(ast.getLHS());
      store(*rhs);
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::ADD: {
      dispatch(ast.getLHS());
      sema.expectRValue();
      Operand *lhs = tmpOperand;
      dispatch(ast.getRHS());
      sema.expectRValue();
      ir->emitAdd(*lhs, *tmpOperand);
      tmpOperand = &ir.getDef();
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::EQ: {
      dispatch(ast.getLHS());
      sema.expectRValue();
      Operand *lhs = tmpOperand;
      dispatch(ast.getRHS());
      sema.expectRValue();
      ir->emitCmp(BrCond::eq(), *lhs, *tmpOperand);
      tmpOperand = &ir.getDef();
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    default:
      assert(false && "unsupported");
      break;
    }
  }

  void visitIfSt(IfStAST &ast) {
    dispatch(ast.getExpression());
    sema.expectRValue();
    sema.expectTypeKind(Type::BOOL);
    Operand &condExpr = *tmpOperand;
    Block &src = ir.endBlock();

    ir.startBlock();
    dispatch(ast.getStatement());
    Block &dstTrue = ir.endBlock();

    Block *dstFalse = nullptr;
    if (ast.hasElseStatement()) {
      dstFalse = &ir.startBlock();
      dispatch(ast.getElseStatement());
      ir.endBlock();
    }

    Block &dst = ir.startBlock();
    ir.setBlock(dstTrue);
    ir->emitBr(dst);
    if (dstFalse) {
      ir.setBlock(dstFalse);
      ir->emitBr(dst);
    } else {
      dstFalse = &dst;
    }
    ir.setBlock(src);
    ir->emitBrCond(condExpr, dstTrue, *dstFalse);
    ir.setBlock(dst);
  }

  void visitWhileSt(WhileStAST &ast) {}

  void error(std::string_view err) {
    std::cerr << "[IRGen] " << err << "\n";
    assert(false);
    exit(1);
  }

  void store(Operand &operand) {
    sema.expectLValue();
    if (sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL) {
      auto &s = storage.get(tmpSymbol->getId());
      if (s.kind == StorageBuilder::SSA) {
        SSABuilder::store(tmpSymbol->getId(), operand, ir.getBlock());
        storage.trackSSADef(s, *tmpOperand);
        return;
      }
    }
    ir->emitStore(*tmpOperand, operand);
  }

  ExpressionSemantics::Result semanticConvLValue() {
    if (sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL) {
      auto &s = storage.get(tmpSymbol->getId());
      if (s.kind == StorageBuilder::SSA) {
        tmpOperand = SSABuilder::load(tmpSymbol->getId(), ir.getBlock());
        assert(tmpOperand);
        return ExpressionSemantics::SUCCESS;
      }
    }

    ir->emitLoad(StorageBuilder::memType(*sema.getType()), *tmpOperand);
    tmpOperand = &ir.getDef();
    return ExpressionSemantics::SUCCESS;
  }

  ExpressionSemantics::Result semanticConvBool() {
    assert(tmpOperand->ssaDefType().getKind() == SSAType::INT);
    ir->emitConstInt(static_cast<IntSSAType &>(tmpOperand->ssaDefType()), 0);
    ir->emitCmp(BrCond::ne(), *tmpOperand, ir.getDef());
    tmpOperand = &ir.getDef();
    return ExpressionSemantics::SUCCESS;
  }

  void semanticError(ExpressionSemantics::Result err) {
    error("Semantics error");
  }

  Operand *tmpOperand = nullptr;
  Symbol *tmpSymbol = nullptr;
  IRBuilder ir;
  SymbolTable sym;
  ExpressionSemantics sema;
  StorageBuilder storage;
};
} // namespace

std::unique_ptr<Program> IRGenAST(TranslationUnitAST &ast, SymbolTable &sym) {
  auto gen = IRGenASTVisitor(sym);
  sym.clearScopeStack();
  gen.ir.startProgram();
  gen.dispatch(ast);
  return gen.ir.endProgram();
}
