#pragma once
#include "c/Symbol.h"
#include "c/Type.h"
#include "support/RefCount.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

namespace c {

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
    PLUS,
    MINUS,
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
    ACCESS_MEMBER_DEREF,
    ACCESS_MEMBER,
    ACCESS_ARRAY,
    FUNCTION_CALL,
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
    case EMPTY:
      return "Empty";
    case ASSIGN:
      return "Assign";
    case ASSIGN_ADD:
      return "AssignAdd";
    case ASSIGN_SUB:
      return "AssignSub";
    case ASSIGN_MUL:
      return "AssignMul";
    case ASSIGN_DIV:
      return "AssignDiv";
    case ASSIGN_MOD:
      return "AssignMod";
    case ASSIGN_AND:
      return "AssignAnd";
    case ASSIGN_OR:
      return "AssignOr";
    case ASSIGN_XOR:
      return "AssignXor";
    case ASSIGN_LSHIFT:
      return "AssignShiftLeft";
    case ASSIGN_RSHIFT:
      return "AssignShiftRight";
    case DIV:
      return "Division";
    case MOD:
      return "Modulo";
    case BIT_AND:
      return "BitAnd";
    case BIT_OR:
      return "BitOr";
    case BIT_NOT:
      return "BitNot";
    case BIT_XOR:
      return "BitXor";
    case LSHIFT:
      return "ShiftLeft";
    case RSHIFT:
      return "ShiftRight";
    case LOG_AND:
      return "LogicalAnd";
    case LOG_OR:
      return "LogicalOr";
    case LOG_NOT:
      return "LogicalNot";
    case INC_POST:
      return "PostIncrement";
    case DEC_PRE:
      return "PreDecrement";
    case DEC_POST:
      return "PostDecrement";
    case EQ:
      return "==";
    case NEQ:
      return "!=";
    case LT:
      return "<";
    case GT:
      return ">";
    case LTE:
      return "<=";
    case GTE:
      return ">=";
    case ST_COMPOUND:
      return "CompoundStatement";
    case ST_IF:
      return "if";
    case ST_WHILE:
      return "while";
    case DECLARATOR:
      return "Declarator";
    case DECLARATION:
      return "Declaration";
    case FUNCTION_DEFINITION:
      return "FunctionDefinition";
    case TRANSLATION_UNIT:
      return "TranslationUnit";
    case ACCESS_MEMBER_DEREF:
      return "DerefMemberAccess";
    case ACCESS_MEMBER:
      return "MemberAccess";
    case ACCESS_ARRAY:
      return "ArrAccess";
    case FUNCTION_CALL:
      return "FunctionCall";
    case PLUS:
      return "Plus";
    case MINUS:
      return "Minus";
    }
    return "unnamed";
  }

  AST(Kind kind) : kind(kind) {}
  virtual ~AST() = default;

  Kind getKind() { return kind; }

private:
  Kind kind;
};

class UnopAST : public AST {
public:
  UnopAST(Kind kind, Ptr child) : AST(kind), child(std::move(child)) {}

  AST &getSubExpression() { return *child; }

  std::unique_ptr<AST> child;
};

class BinopAST : public AST {
public:
  BinopAST(Kind kind, Ptr lhs, Ptr rhs)
      : AST(kind), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  AST &getLHS() { return *lhs; }
  AST &getRHS() { return *rhs; }

  Ptr lhs, rhs;
};

class NumAST : public AST {
public:
  int64_t num;
  NumAST(uint64_t num) : AST(NUM), num(num) {}
};

class MemberAccessAST : public AST {
public:
  MemberAccessAST(Kind kind, Ptr child, std::string_view ident)
      : AST(kind), child(std::move(child)), ident(ident) {}
  std::string_view ident;
  Ptr child;
};

class ArrAccessAST : public AST {
public:
  ArrAccessAST(Ptr arr, Ptr expr)
      : AST(ACCESS_ARRAY), arr(std::move(arr)), expr(std::move(expr)) {}
  Ptr arr;
  Ptr expr;
};

class FunctionCallAST : public AST {
public:
  FunctionCallAST() : AST(FUNCTION_CALL) {}
};

class VarAST : public AST {
public:
  std::string_view ident;
  VarAST(std::string_view varName) : AST(VAR), ident(varName) {}
};

class CompoundStAST : public AST {
public:
  CompoundStAST() : AST(ST_COMPOUND), scope(Scope::BLOCK) {}
  std::vector<Ptr> children;
  Scope scope;
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

  Ptr expr, st, stElse;
};

class WhileStAST : public AST {
public:
  WhileStAST(Ptr expr, Ptr st)
      : AST(ST_WHILE), expr(std::move(expr)), st(std::move(st)) {}

  AST &getExpression() { return *expr; }
  AST &getStatement() { return *st; }

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
  std::vector<std::pair<DeclaratorAST, Ptr>> declarators;
};

class FunctionDefinitionAST : public AST {
public:
  FunctionDefinitionAST(DeclSpec spec, DeclaratorAST decl)
      : AST(FUNCTION_DEFINITION), spec(std::move(spec)), decl(std::move(decl)),
        funcScope(Scope::FUNC), blockScope(Scope::BLOCK) {}
  DeclSpec spec;
  DeclaratorAST decl;
  Ptr st;
  Scope funcScope;
  Scope blockScope;

  CompoundStAST &getStatement() {
    return *static_cast<CompoundStAST *>(st.get());
  }
  FuncType &getType() { return *static_cast<FuncType *>(decl.type.get()); }
};

class TranslationUnitAST : public AST {
public:
  TranslationUnitAST() : AST(TRANSLATION_UNIT), scope(Scope::FILE) {}

  std::vector<Ptr> declarations;
  Scope scope;
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
} // namespace c
