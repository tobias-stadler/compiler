#pragma once
#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "support/RefCount.h"
#include <cassert>
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
    FUNCTION_DEFINITION,
    TRANSLATION_UNIT,
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
  UnopAST(Kind kind, Ptr child)
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
  std::string_view ident;
  VarAST(std::string_view varName) : AST(VAR), ident(varName) {}
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

  DeclSpec spec;
  std::vector<DeclaratorAST> declarators;
};

class FunctionDefinitionAST : public AST {
public:
  FunctionDefinitionAST(DeclSpec spec, DeclaratorAST decl, Ptr st)
      : AST(FUNCTION_DEFINITION), spec(std::move(spec)), decl(std::move(decl)),
        st(std::move(st)) {}
  DeclSpec spec;
  DeclaratorAST decl;
  Ptr st;

  CompoundStAST &getStatement() {
    return *static_cast<CompoundStAST *>(st.get());
  }
  FuncType &getType() { return *static_cast<FuncType *>(decl.type.get()); }
};

class TranslationUnitAST : public AST {
public:
  TranslationUnitAST() : AST(TRANSLATION_UNIT) {}

  std::vector<Ptr> declarations;
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

  T &&moveRes() { return std::move(res()); }

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
