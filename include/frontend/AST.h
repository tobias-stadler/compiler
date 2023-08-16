#pragma once
#include "frontend/Type.h"
#include "support/RefCount.h"
#include <array>
#include <assert.h>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

class AST {
public:
  using Ptr = std::unique_ptr<AST>;
  enum Kind {
    EMPTY,
    ASSIGN,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_MUL,
    ASSIGN_DIV,
    ASSIGN_MOD,
    ASSIGN_AND,
    ASSIGN_OR,
    ASSIGN_XOR,
    ASSIGN_LSHIFT,
    ASSIGN_RSHIFT,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    BIT_AND,
    BIT_OR,
    BIT_NOT,
    BIT_XOR,
    LSHIFT,
    RSHIFT,
    LOG_AND,
    LOG_OR,
    LOG_NOT,
    INC_PRE,
    INC_POST,
    DEC_PRE,
    DEC_POST,
    DEREF,
    ADDR,
    VAR,
    NUM,
    EQ,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,
    ST_COMPOUND,
    ST_IF,
    ST_WHILE,
    DECLARATOR,
    DECLARATION,
    FUNCTION
  };

  static constexpr const char *kindName(Kind kind) {
    switch (kind) {
    case MUL:
      return "Mul";
    case ADD:
      return "Add";
    case SUB:
      return "Sub";
    case VAR:
      return "Var";
    case NUM:
      return "Num";
    case INC_PRE:
      return "IncPre";
    case DEREF:
      return "Deref";
    case ADDR:
      return "Addr";
    default:
      return "Unnamed";
    }
  }

  AST(Kind kind) : kind(kind) {}
  virtual ~AST() = default;

  Kind getKind() { return kind; }

private:
  Kind kind;
};

class ExpressionAST : public AST {
public:
  ExpressionAST(Kind kind) : AST(kind) {}
};

class UnopAST : public ExpressionAST {
public:
  std::unique_ptr<AST> child;
  UnopAST(Kind kind, std::unique_ptr<AST> child)
      : ExpressionAST(kind), child(std::move(child)) {}

  AST &getSubExpression() { return *child; }
};

class BinopAST : public ExpressionAST {
public:
  BinopAST(Kind kind, Ptr lhs, Ptr rhs)
      : ExpressionAST(kind), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  AST &getLHS() { return *lhs; }
  AST &getRHS() { return *rhs; }

private:
  Ptr lhs;
  Ptr rhs;
};

class NumAST : public AST {
public:
  int64_t num;
  NumAST(uint64_t num) : AST(NUM), num(num) {}
};

class VarAST : public AST {
public:
  std::string_view varName;
  VarAST(std::string_view varName) : AST(VAR), varName(varName) {}
};

class CompoundStAST : public AST {
public:
  CompoundStAST() : AST(ST_COMPOUND) {}
  std::vector<Ptr> children;
};

class IfStAST : public AST {
public:
  IfStAST(Ptr expr, Ptr st, Ptr stElse = nullptr)
      : AST(ST_IF), expr(std::move(expr)), st(std::move(st)),
        stElse(std::move(stElse)) {}

  AST &getExpression() { return *expr; }
  AST &getStatement() { return *st; }
  AST &getElseStatement() { return *stElse; }
  bool hasElseStatement() { return bool(stElse); }

private:
  Ptr expr, st, stElse;
};

class WhileStAST : public AST {
public:
  WhileStAST(Ptr expr, Ptr st)
      : AST(ST_WHILE), expr(std::move(expr)), st(std::move(st)) {}

  AST &getExpression() { return *expr; }
  AST &getStatement() { return *st; }

private:
  Ptr expr, st;
};

class DeclSpec {
public:
  DeclSpec() = default;
  DeclSpec(Symbol::Kind symbolKind, Type::Kind typeKind,
           Type::Qualifier qualifier)
      : symbolKind(symbolKind), typeKind(typeKind), qualifier(qualifier) {}
  Symbol::Kind symbolKind = Symbol::EMPTY;
  Type::Kind typeKind = Type::EMPTY;
  Type::Qualifier qualifier;

  CountedPtr<BasicType> createType() {
    return BasicType::create(typeKind, qualifier);
  }
};

class ASTError {
public:
  enum Kind {
    EMPTY,
    NOP,
    EXPECTED_TOKEN,
  };
  ASTError(Kind kind) : kind(kind) {}
  Kind getKind() { return kind; }
  bool isEmpty() { return kind == EMPTY; };
  bool isNop() { return kind == NOP; };

private:
  Kind kind;
};

template <typename T> class ASTResult {
public:
  ASTResult(T &&res) : mRes(std::forward<T>(res)), mErr(ASTError::EMPTY) {}
  ASTResult(ASTError err) : mErr(err) {}

  explicit operator bool() { return mErr.isEmpty(); }

  ASTError &err() {
    assert(!mErr.isEmpty() && "Invalid ASTError accessed");
    return mErr;
  }

  T &res() {
    assert(mErr.isEmpty() && "Invalid ASTResult accessed");
    return mRes;
  }

  bool isNop() { return mErr.isNop(); }

  T &operator*() { return res(); }
  T *operator->() { return &res(); }

protected:
  T mRes;
  ASTError mErr;
};

class ASTPtrResult : public ASTResult<std::unique_ptr<AST>> {
public:
  ASTPtrResult() : ASTResult(ASTError::NOP) {}
  ASTPtrResult(std::nullptr_t) : ASTResult(ASTError::NOP) {}
  ASTPtrResult(AST *ast) : ASTResult(std::unique_ptr<AST>(ast)) {
    assert(ast && "Use NOP instead of nullptr");
  }
  ASTPtrResult(ASTError err) : ASTResult(err) {}

  operator AST::Ptr &&() {
    assert(mErr.isEmpty() && "Invalid ASTResult accessed");
    return std::move(mRes);
  }

  template <class U>
  ASTPtrResult(std::unique_ptr<U> &&ast) : ASTResult(std::move(ast)) {}
};

class DeclaratorAST : public AST {
public:
  DeclaratorAST() : AST(DECLARATOR) {}

  DeclaratorAST(CountedPtr<DerivedType> type)
      : AST(DECLARATOR), typeBase(type.get()), type(std::move(type)) {}

  DeclaratorAST(CountedPtr<Type> type, DerivedType *typeBase,
                std::string_view ident = std::string_view())
      : AST(DECLARATOR), typeBase(typeBase), type(std::move(type)),
        ident(ident) {}

  void spliceEnd(DeclaratorAST o) {
    assert(canSplice() && "Expected spliceable DeclaratorAST");
    if (!o.ident.empty()) {
      ident = o.ident;
    }
    if (type) {
      typeBase->setBaseType(std::move(o.type));
    } else {
      type = std::move(o.type);
    }
    typeBase = o.typeBase;
  }

  bool canSplice() { return bool(type) == bool(typeBase); }

  DerivedType *typeBase;
  CountedPtr<Type> type;
  std::string_view ident;
};

class DeclarationAST : public AST {
public:
  DeclarationAST() : AST(DECLARATION) {}

  std::vector<DeclaratorAST> declarators;
};

#define VISIT_DELEGATE(AstTy)                                                  \
  impl()->visit##AstTy(*static_cast<AstTy##AST *>(&ast));

template <class D> class ASTVisitor {
public:
  void run(AST *ast) { dispatch(ast); }

protected:
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
      impl()->visit(ast);
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
    }
  }

  D *impl() { return static_cast<D *>(this); }

  void visit(AST &ast) {}
  void visitNum(NumAST &ast) { impl()->visit(ast); }
  void visitVar(VarAST &ast) { impl()->visit(ast); }
  void visitBinop(BinopAST &ast) { impl()->visit(ast); }
  void visitCompoundSt(CompoundStAST &ast) { impl()->visit(ast); }
  void visitIfSt(IfStAST &ast) { impl()->visit(ast); }
  void visitWhileSt(WhileStAST &ast) { impl()->visit(ast); }
  void visitDeclarator(DeclaratorAST &ast) { impl()->visit(ast); }
};

class PrintASTVisitor : public ASTVisitor<PrintASTVisitor> {
public:
  void run(AST *ast) {
    dispatch(ast);
    std::cout << "\n";
  }

  void visit(AST &ast) { std::cout << AST::kindName(ast.getKind()); }

  void visitVar(VarAST &ast) { std::cout << "Var(" << ast.varName << ")"; }

  void visitNum(NumAST &ast) { std::cout << "Num(" << ast.num << ")"; }

  void visitUnop(UnopAST &ast) {
    std::cout << "Unop(" << AST::kindName(ast.getKind());
    std::cout << ",";
    dispatch(ast.getSubExpression());
    std::cout << ")";
  }
  void visitBinop(BinopAST &ast) {
    std::cout << "Binop(" << AST::kindName(ast.getKind());
    std::cout << ",";
    dispatch(ast.getLHS());
    std::cout << ",";
    dispatch(ast.getRHS());
    std::cout << ")";
  }

  void visitCompoundSt(CompoundStAST &ast) {
    std::cout << "{\n";
    for (auto &child : ast.children) {
      dispatch(*child);
      std::cout << ";\n";
    }
    std::cout << "}";
  }

  void visitIfSt(IfStAST &ast) {
    std::cout << "if(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
    if (ast.hasElseStatement()) {
      std::cout << "\nelse\n";
      dispatch(ast.getElseStatement());
    }
  }

  void visitWhileSt(WhileStAST &ast) {
    std::cout << "while(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
  }

  void visitDeclarator(DeclaratorAST &ast) {
    std::cout << ast.ident << " is ";
    printType(ast.type.get());
  }

  void printType(Type *type) {
    if (!type) {
      std::cout << "NULL";
      return;
    }
    switch (type->getKind()) {
    case Type::SINT:
      std::cout << "signed int";
      break;
    case Type::PTR:
      std::cout << "pointer to ";
      printType(&static_cast<DerivedType *>(type)->getBaseType());
      break;
    case Type::FUNC: {
      std::cout << "function(";
      bool first = true;
      FuncType *p = static_cast<FuncType *>(type);
      for (auto &t : p->getParamTypes()) {
        if (!first) {
          std::cout << ", ";
        }
        first = false;
        printType(t.get());
      }
      std::cout << ") returning ";
      printType(&p->getBaseType());
      break;
    }
    default:
      std::cout << "unnamed";
    }
  }
};
