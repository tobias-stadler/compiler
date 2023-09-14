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
#include "support/RefCount.h"
#include <cassert>
#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

namespace {

constexpr BrCond irBrCond(AST::Kind kind, bool isSigned) {
  switch (kind) {
  case AST::EQ:
    return BrCond::eq();
  case AST::NEQ:
    return BrCond::ne();
  case AST::LT:
    return isSigned ? BrCond::lt() : BrCond::ltu();
  case AST::GT:
    return isSigned ? BrCond::gt() : BrCond::gtu();
  case AST::LTE:
    return isSigned ? BrCond::le() : BrCond::leu();
  case AST::GTE:
    return isSigned ? BrCond::ge() : BrCond::geu();
  default:
    __builtin_unreachable();
  }
}

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

  StorageInfo &declareLocal(Symbol &s) {
    auto [it, succ] = slots.emplace(s.getId(), s);
    return it->second;
  }

  void allocateStack(StorageInfo &s) {
    assert(s.kind == EMPTY);
    SSAType &ssaType = Type::irType(s.symbol->getType()->getKind());
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

class ExprState {
public:
  ExprState(Symbol *tmpSymbol, Operand *tmpOperand, ExpressionSemantics sema)
      : tmpSymbol(tmpSymbol), tmpOperand(tmpOperand), sema(std::move(sema)) {}

  Symbol *tmpSymbol;
  Operand *tmpOperand;
  ExpressionSemantics sema;
};

class IRGenASTVisitor : public ASTVisitor<IRGenASTVisitor>,
                        public ExpressionSemantics::Handler {
public:
  IRGenASTVisitor(SymbolTable &sym) : sema(*this), storage(ir), sym(sym) {}

  void visitTranslationUnit(TranslationUnitAST &ast) {
    sym.pushScope(ast.scope);
    for (auto &d : ast.declarations) {
      dispatch(d.get());
    }
    sym.popScope();
  }

  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    ir.createAndSetFunction();
    storage.setFunction(ir.getFunc());
    ir.createAndSetBlock();
    SSABuilder::sealBlock(ir.getBlock());
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
    SSARenumberTable::destroy(ir.getFunc());
  }

  void visitDeclaration(DeclarationAST &ast) {
    if (sym.scope().isFile()) {
      assert(false && "Global declaration unsupported");
    } else if (sym.scope().isBlock()) {
      for (auto &[d, initializer] : ast.declarators) {
        auto *s = sym.getSymbol(d.ident);
        assert(s && "Symbol undeclared");
        auto &sInfo = storage.declareLocal(*s);
        if (s->isAddrTaken()) {
          storage.allocateStack(sInfo);
        } else {
          storage.allocateSSA(sInfo);
        }
        if (initializer) {
          dispatch(initializer.get());
          sema.expectRValue();
          sema.expectTypeKind(sInfo.symbol->getType()->getKind());
          auto rhs = saveExprState();
          exprStateFromStorageInfo(sInfo);
          sema.expectLValue();
          store(*rhs.tmpOperand);
        }
      }
    }
  }

  void exprStateFromStorageInfo(StorageBuilder::StorageInfo &s) {
    tmpSymbol = s.symbol;
    sema.fromSymbol(*s.symbol);
    tmpOperand = s.ptr;
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
    sema.fromType(BasicType::create(Type::SINT));
  }

  void visitVar(VarAST &ast) {
    tmpSymbol = sym.getSymbol(ast.ident);
    if (!tmpSymbol) {
      error("Symbol undeclared");
    }
    exprStateFromStorageInfo(storage.get(tmpSymbol->getId()));
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
    case AST::LOG_NOT: {
      dispatch(ast.getSubExpression());
      sema.expectRValue();
      sema.expectTypeKind(Type::BOOL);
      ir->emitNot(*tmpOperand);
      tmpOperand = &ir.getDef();
      break;
    }
    case AST::BIT_NOT: {
      dispatch(ast.getSubExpression());
      sema.expectRValue();
      sema.expectTypeKind(
          ExpressionSemantics::intPromotion(sema.getType()->getKind()));
      ir->emitNot(*tmpOperand);
      tmpOperand = &ir.getDef();
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
      auto rhs = saveExprState();

      dispatch(ast.getLHS());
      sema.expectLValue();
      auto lhs = saveExprState();

      restoreExprState(std::move(rhs));
      sema.expectTypeKind(lhs.sema.getType()->getKind());
      rhs = saveExprState();

      restoreExprState(std::move(lhs));
      store(*rhs.tmpOperand);
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::ADD:
    case AST::SUB:
    case AST::MUL:
    case AST::BIT_AND:
    case AST::BIT_OR:
    case AST::BIT_XOR: {
      auto [lhs, rhs] = usualArithBinop(ast);
      switch (ast.getKind()) {
      case AST::ADD:
        ir->emitAdd(*lhs, *rhs);
        break;
      case AST::SUB:
        ir->emitSub(*lhs, *rhs);
        break;
      case AST::MUL:
        ir->emitMul(*lhs, *rhs, Type::isSigned(sema.getType()->getKind()));
        break;
      case AST::BIT_AND:
        ir->emitAnd(*lhs, *rhs);
        break;
      case AST::BIT_OR:
        ir->emitOr(*lhs, *rhs);
        break;
      case AST::BIT_XOR:
        ir->emitXor(*lhs, *rhs);
        break;
      default:
        __builtin_unreachable();
      }
      tmpOperand = &ir.getDef();
      sema.setCategory(ExpressionSemantics::RVALUE);
      break;
    }
    case AST::EQ:
    case AST::NEQ:
    case AST::LT:
    case AST::LTE:
    case AST::GT:
    case AST::GTE: {
      auto [lhs, rhs] = usualArithBinop(ast);
      ir->emitCmp(
          irBrCond(ast.getKind(), Type::isSigned(sema.getType()->getKind())),
          *lhs, *rhs);
      tmpOperand = &ir.getDef();
      sema.fromType(BasicType::create(Type::BOOL));
      break;
    }
    default:
      assert(false && "unsupported");
      break;
    }
  }

  std::pair<Operand *, Operand *> usualArithBinop(BinopAST &ast) {
    dispatch(ast.getLHS());
    sema.expectRValue();
    auto lhs = saveExprState();

    dispatch(ast.getRHS());
    sema.expectRValue();
    Type::Kind commonTyKind = ExpressionSemantics::usualArithConv(
        lhs.sema.getType()->getKind(), sema.getType()->getKind());
    sema.expectTypeKind(commonTyKind);
    auto rhs = saveExprState();

    restoreExprState(std::move(lhs));
    sema.expectTypeKind(commonTyKind);
    return {tmpOperand, rhs.tmpOperand};
  }

  Operand *boolExpression(AST &expr) {
    dispatch(expr);
    sema.expectRValue();
    sema.expectTypeKind(Type::BOOL);
    return tmpOperand;
  }

  void visitIfSt(IfStAST &ast) {
    Block &trueBlock = ir.createBlock();
    Block *falseBlock = ast.hasElseStatement() ? &ir.createBlock() : nullptr;
    Block &exitBlock = ir.createBlock();

    Operand *condExpr = boolExpression(ast.getExpression());
    ir->emitBrCond(*condExpr, trueBlock, falseBlock ? *falseBlock : exitBlock);

    SSABuilder::sealBlock(trueBlock);
    ir.setBlock(trueBlock);
    dispatch(ast.getStatement());
    ir->emitBr(exitBlock);

    if (falseBlock) {
      SSABuilder::sealBlock(*falseBlock);
      ir.setBlock(falseBlock);
      dispatch(ast.getElseStatement());
      ir->emitBr(exitBlock);
    }

    SSABuilder::sealBlock(exitBlock);
    ir.setBlock(exitBlock);
  }

  void visitWhileSt(WhileStAST &ast) {
    Block &hdrBlock = ir.createBlock();
    Block &loopBlock = ir.createBlock();
    Block &exitBlock = ir.createBlock();
    ir->emitBr(hdrBlock);

    ir.setBlock(hdrBlock);
    Operand *condExpr = boolExpression(ast.getExpression());
    ir->emitBrCond(*condExpr, loopBlock, exitBlock);
    SSABuilder::sealBlock(loopBlock);
    ir.setBlock(loopBlock);
    dispatch(ast.getStatement());
    ir->emitBr(hdrBlock);
    SSABuilder::sealBlock(hdrBlock);

    SSABuilder::sealBlock(exitBlock);
    ir.setBlock(exitBlock);
  }

  void error(std::string_view err) {
    std::cerr << "[IRGen] " << err << "\n";
    assert(false);
    exit(1);
  }

  void store(Operand &operand) {
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
        tmpOperand = SSABuilder::load(tmpSymbol->getId(),
                                      Type::irType(sema.getType()->getKind()),
                                      ir.getBlock());
        assert(tmpOperand);
        return ExpressionSemantics::SUCCESS;
      }
    }

    ir->emitLoad(Type::irType(sema.getType()->getKind()), *tmpOperand);
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

  ExpressionSemantics::Result semanticConvInt(Type::Kind dstKind) {
    ir->emitExtOrTrunc(Type::irType(dstKind), *tmpOperand,
                       Type::isSigned(sema.getType()->getKind()));
    tmpOperand = &ir.getDef();
    // TODO: proper conversion between signed and unsigned
    return ExpressionSemantics::SUCCESS;
  }

  void semanticError(ExpressionSemantics::Result err) {
    error("Semantics error");
  }

  ExprState saveExprState() { return ExprState(tmpSymbol, tmpOperand, sema); }

  void restoreExprState(ExprState &&state) {
    tmpOperand = state.tmpOperand;
    tmpSymbol = state.tmpSymbol;
    sema = std::move(state.sema);
  }

  Operand *tmpOperand = nullptr;
  Symbol *tmpSymbol = nullptr;
  ExpressionSemantics sema;

  IRBuilder ir;
  StorageBuilder storage;
  SymbolTable sym;
};
} // namespace

std::unique_ptr<Program> IRGenAST(TranslationUnitAST &ast, SymbolTable &sym) {
  auto gen = IRGenASTVisitor(sym);
  sym.clearScopeStack();
  gen.ir.startProgram();
  gen.dispatch(ast);
  return gen.ir.endProgram();
}
