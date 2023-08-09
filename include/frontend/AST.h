#pragma once
#include <array>
#include <assert.h>
#include <cstdint>
#include <iostream>
#include <string_view>
#include <utility>
#include <vector>

class AST {
public:
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
    INC_PRE,
    INC_POST,
    DEC_PRE,
    DEC_POST,
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
    DEREF,
    ADDR,
    DECL_PTR,
    DECL_IDENT,
    DECL_FUN,
    DECL_ARR,
  };
  Kind kind;

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
};

template <unsigned N> class ArrAST : public AST {
public:
  std::array<AST *, N> children;

  ArrAST(Kind kind, std::array<AST *, N> children = {})
      : AST(kind), children(std::move(children)) {}

  ArrAST(ArrAST &&o) noexcept { *this = std::move(o); }

  ArrAST &operator=(ArrAST &&o) noexcept {
    std::swap(children, o.children);
    kind = std::exchange(o.kind, EMPTY);
    return *this;
  }

  AST &getChild(unsigned n) {
    assert(children[n] && "Expected child to be non-null");
    return *children[n];
  }

  ~ArrAST() {
    for (AST *child : children) {
      delete child;
    }
  }
};

class VecAST : public AST {
public:
  std::vector<AST *> children;

  VecAST(Kind kind) : AST(kind) {}
  VecAST(Kind kind, std::vector<AST *> children)
      : AST(kind), children(std::move(children)) {}

  VecAST(VecAST &&o) noexcept : AST(EMPTY) { *this = std::move(o); }

  VecAST &operator=(VecAST &&o) noexcept {
    std::swap(children, o.children);
    kind = std::exchange(o.kind, EMPTY);
    return *this;
  }

  ~VecAST() {
    for (AST *child : children) {
      delete child;
    }
  }
};

class UnopAST : public ArrAST<1> {
public:
  UnopAST(Kind kind, AST *child) : ArrAST(kind, {child}) {}

  AST &getSubExpression() { return getChild(0); }
};

class BinopAST : public ArrAST<2> {
public:
  BinopAST(Kind kind, AST *lhs, AST *rhs) : ArrAST(kind, {lhs, rhs}) {}

  AST &getLHS() { return getChild(0); }
  AST &getRHS() { return getChild(1); }
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

class CompoundStAST : public VecAST {
public:
  CompoundStAST() : VecAST(ST_COMPOUND) {}
};

class IfStAST : public ArrAST<3> {
public:
  IfStAST(AST *expr, AST *st, AST *stElse = nullptr)
      : ArrAST(ST_IF, {expr, st, stElse}) {}

  AST &getExpression() { return getChild(0); }
  AST &getStatement() { return getChild(1); }

  bool hasElse() { return children[2]; }

  AST &getElseStatement() { return getChild(2); }
};

class WhileStAST : public ArrAST<2> {
public:
  WhileStAST(AST *expr, AST *st) : ArrAST(ST_WHILE, {expr, st}) {}

  AST &getExpression() { return getChild(0); }
  AST &getStatement() { return getChild(1); }
};

class IdentDeclAST : public AST {
public:
  std::string_view varName;
  IdentDeclAST(std::string_view varName) : AST(DECL_IDENT), varName(varName) {}
};

class PtrDeclAST : public ArrAST<1> {
public:
  PtrDeclAST(AST *decl) : ArrAST(DECL_PTR, {decl}) {}
  AST &getSubDecl() { return getChild(0); }
};

class FunDeclAST : public ArrAST<1> {};

class ArrDeclAST : public ArrAST<1> {};

class TypeSpecAST : public ArrAST<1> {};

#define VISIT_DELEGATE(AstTy)                                                  \
  impl()->visit##AstTy(*static_cast<AstTy##AST *>(ast));

template <class D> class ASTVisitor {
public:
  void run(AST *ast) { dispatch(ast); }

protected:
  void dispatch(AST &ast) { dispatch(&ast); }

  void dispatch(AST *ast) {
    if (!ast)
      return;
    switch (ast->kind) {
    case AST::EMPTY:
      break;
    default:
      impl()->visit(*ast);
      break;
    case AST::MUL:
    case AST::ADD:
    case AST::SUB:
    case AST::DIV:
    case AST::ASSIGN:
    case AST::BIT_AND:
    case AST::BIT_OR:
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
    }
  }

  template <int N> void dispatchChildren(ArrAST<N> &ast) {
    for (AST *child : ast.children) {
      dispatch(child);
    }
  }

  D *impl() { return static_cast<D *>(this); }

  void dispatchChildren(VecAST &ast) {
    for (AST *child : ast.children) {
      dispatch(child);
    }
  }

  void visit(AST &ast) {}
  void visitNum(NumAST &ast) { impl()->visit(ast); }
  void visitVar(VarAST &ast) { impl()->visit(ast); }
  void visitBinop(BinopAST &ast) { impl()->visit(ast); }
  void visitCompoundSt(CompoundStAST &ast) { impl()->visit(ast); }
  void visitIfSt(IfStAST &ast) { impl()->visit(ast); }
  void visitWhileSt(WhileStAST &ast) { impl()->visit(ast); }
};

class PrintASTVisitor : public ASTVisitor<PrintASTVisitor> {
public:
  void run(AST *ast) {
    dispatch(ast);
    std::cout << "\n";
  }

  void visit(AST &ast) { std::cout << AST::kindName(ast.kind); }

  void visitVar(VarAST &ast) { std::cout << "Var(" << ast.varName << ")"; }

  void visitNum(NumAST &ast) { std::cout << "Num(" << ast.num << ")"; }

  void visitUnop(UnopAST &ast) {
    std::cout << "Unop(" << AST::kindName(ast.kind);
    std::cout << ",";
    dispatch(ast.getSubExpression());
    std::cout << ")";
  }
  void visitBinop(BinopAST &ast) {
    std::cout << "Binop(" << AST::kindName(ast.kind);
    for (AST *child : ast.children) {
      std::cout << ",";
      dispatch(child);
    }
    std::cout << ")";
  }

  void visitCompoundSt(CompoundStAST &ast) {
    std::cout << "{\n";
    for (AST *child : ast.children) {
      dispatch(child);
      std::cout << ";\n";
    }
    std::cout << "}";
  }

  void visitIfSt(IfStAST &ast) {
    std::cout << "if(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
    if (ast.hasElse()) {
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
};
