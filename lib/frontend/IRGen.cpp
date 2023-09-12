#include "frontend/IRGen.h"
#include "frontend/AST.h"
#include "frontend/ASTVisitor.h"
#include "frontend/Semantics.h"
#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "ir/IR.h"
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
  enum StorageKind {
    EMPTY,
    SSA,
    STACK,
  };

  class StorageInfo {
  public:
    StorageInfo(Symbol &symbol) : symbol(&symbol) {}
    // FIXME: symbol becomes dangling when symbol table scope ends
    Symbol *symbol;
    StorageKind kind = EMPTY;
    Operand *ptr = nullptr;
    std::vector<Operand *> ssaDefs;
    std::vector<Operand *> ssaUsedDefs;
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
    SSAType &ssaType = memType(*s.symbol->getType());
    Operand &ptr =
        InstrBuilder(currFunc->getEntry().getSentryBegin().getNextNode())
            .emitAlloca(ssaType, 1)
            .getDef();
    s.kind = STACK;
    s.ptr = &ptr;
  }

  void setFunction(Function &func) {
    slots.clear();
    currFunc = &func;
  }

  void allocateSSA(StorageInfo &s) {
    assert(s.kind == EMPTY);
    s.kind = SSA;
    s.ptr = nullptr;
  }

  void trackSSADef(StorageInfo &s, Operand &operand) {
    assert(s.kind == SSA);
    s.ssaDefs.push_back(&operand);
  }
  void trackSSAUsedDef(StorageInfo &s, Operand &operand) {
    assert(s.kind == SSA);
    s.ssaUsedDefs.push_back(&operand);
  }

  void downgradeSSAToStack(StorageInfo &s) {
    assert(s.kind == SSA);
    allocateStack(s);
    for (auto *op : s.ssaUsedDefs) {
      for (auto &use : op->ssaDef()) {
        auto b = InstrBuilder(use.getParent());
        b.emitLoad(s.ptr->getParent().getOperand(1).type(), *s.ptr);
        use.ssaUseReplace(b.getDef());
      }
    }
    for (auto *op : s.ssaDefs) {
      auto b = InstrBuilder(op->getParent().getNextNode());
      b.emitStore(*s.ptr, *op);
    }
  }

  StorageInfo &get(SSASymbolId id) {
    auto it = slots.find(id);
    assert(it != slots.end());
    return it->second;
  }

private:
  Function *currFunc;
  std::unordered_map<SSASymbolId, StorageInfo> slots;
};

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
    storage.setFunction(ssa.getFunc());
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
        auto *s =
            sym.declareSymbol(d.ident, Symbol(Symbol::AUTO, std::move(d.type)));
        if (!s) {
          error("Symbol redeclared");
        }
        storage.allocateSSA(storage.declareLocal(*s));
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

  void visitUnop(UnopAST &ast) {
    switch (ast.getKind()) {
    case AST::ADDR: {
      dispatch(ast.getSubExpression());
      if (sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL) {
        auto &s = storage.get(tmpSymbol->getId());
        tmpOperand = s.ptr;
      }
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
      sema.expectLValue();
      // TODO: mem store
      ssa.storeSSA(tmpSymbol->getId(), *rhs);
      storage.trackSSADef(storage.get(tmpSymbol->getId()), *tmpOperand);
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
    assert(false);
    exit(1);
  }

  ExpressionSemantics::Result semanticConvLValue() {
    if (sema.getCategory() == ExpressionSemantics::LVALUE_SYMBOL) {
      auto &s = storage.get(tmpSymbol->getId());
      if (s.kind == StorageBuilder::SSA) {
        tmpOperand = ssa.loadSSA(tmpSymbol->getId());
        if (!tmpOperand) {
          return ExpressionSemantics::ERROR;
        }
        storage.trackSSAUsedDef(s, *tmpOperand);
      } else if (s.kind == StorageBuilder::STACK) {
        ssa->emitLoad(StorageBuilder::memType(*sema.getType()), *s.ptr);
        tmpOperand = &ssa.getDef();
      }
      return ExpressionSemantics::SUCCESS;
    } else if (sema.getCategory() == ExpressionSemantics::LVALUE_MEM) {
      ssa->emitLoad(StorageBuilder::memType(*sema.getType()), *tmpOperand);
      tmpOperand = &ssa.getDef();
      return ExpressionSemantics::SUCCESS;
    }
    return ExpressionSemantics::ERROR;
  }

  ExpressionSemantics::Result semanticSSADowngrade() {
    auto &s = storage.get(tmpSymbol->getId());
    if (s.kind == StorageBuilder::SSA) {
      storage.downgradeSSAToStack(s);
    }
    return ExpressionSemantics::SUCCESS;
  }

  void semanticError(ExpressionSemantics::Result err) {
    error("Semantics error");
  }

  Operand *tmpOperand = nullptr;
  Symbol *tmpSymbol = nullptr;
  StorageBuilder storage;

  SymbolTable sym;
  SSABuilder ssa;
  ExpressionSemantics sema;
};
} // namespace

std::unique_ptr<Program> IRGenAST(AST &ast) {
  auto gen = IRGenASTVisitor();
  gen.ssa.startProgram();
  gen.dispatch(ast);
  return gen.ssa.endProgram();
}
