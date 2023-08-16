#pragma once
#include "support/RefCount.h"
#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

class Type : public IntrusiveCountedPtrBase {
public:
  enum Kind {
    EMPTY,
    BASIC_START,
    VOID,
    SCHAR,
    SSHORT,
    SINT,
    SLONG,
    SLONGLONG,
    UCHAR,
    USHORT,
    UINT,
    ULONG,
    ULONGLONG,
    BASIC_END,
    DERIVED_START,
    ARR,
    STRUCT,
    FUNC,
    PTR,
    DERIVED_END
  };
  class Qualifier {
  public:
    Qualifier(bool qConst = false, bool qVolatile = false,
              bool qRestrict = false, bool qAtomic = false)
        : qConst(qConst), qVolatile(qVolatile), qRestrict(qRestrict),
          qAtomic(qAtomic) {}
    bool isConst() { return qConst; }
    bool isVolatile() { return qVolatile; }
    bool isRestrict() { return qRestrict; }
    bool isAtomic() { return qAtomic; }
    bool onlyConst() { return !(qRestrict || qVolatile || qAtomic); }

  private:
    bool qConst, qVolatile, qRestrict, qAtomic;
  };
  virtual ~Type() = default;

  Kind getKind() { return kind; }

  static bool isBasic(Kind kind) {
    return kind > BASIC_START && kind < BASIC_END;
  }

  static bool isDerived(Kind kind) {
    return kind > DERIVED_START && kind < DERIVED_END;
  }

  static constexpr int NUM_BASIC = BASIC_END - BASIC_START - 1;

protected:
  Type(Kind kind, Qualifier qualifier) : kind(kind), qualifier(qualifier) {}

private:
  Kind kind;
  Qualifier qualifier;
};

class DerivedType : public Type {
public:
  DerivedType(Kind kind, CountedPtr<Type> baseType, Qualifier qualifier)
      : Type(kind, qualifier), baseType(baseType) {}

  Type &getBaseType() { return *baseType; }

  void setBaseType(CountedPtr<Type> newBase) { baseType = std::move(newBase); }

private:
  CountedPtr<Type> baseType;
};

class BasicType : public Type {
public:
  BasicType(Kind kind = VOID, Qualifier qualifier = Qualifier());
  static CountedPtr<BasicType> create(Kind kind, Qualifier qualifier);
};

class PtrType : public DerivedType {
public:
  PtrType(Qualifier qualifier) : DerivedType(PTR, nullptr, qualifier) {}
};

class ArrType : public DerivedType {
public:
  ArrType() : DerivedType(ARR, nullptr, Qualifier()) {}
};

class FuncType : public DerivedType {
public:
  FuncType() : DerivedType(FUNC, nullptr, Qualifier()) {}
  Type &getParamType(size_t i) { return *paramTypes[i]; }
  size_t getNumParams() { return paramTypes.size(); };
  void addParam(CountedPtr<Type> type) {
    paramTypes.push_back(std::move(type));
  }

  auto &getParamTypes() { return paramTypes; }

private:
  std::vector<CountedPtr<Type>> paramTypes;
};

class StructType : public Type {
public:
  StructType() : Type(STRUCT, Qualifier()) {}

  Type *getNamedMemberType(std::string_view v) {
    auto it = memberIndex.find(v);
    if (it == memberIndex.end()) {
      return nullptr;
    }
    return members[it->second].get();
  }

  void addMember(Type *type) { members.push_back(type); }

  bool addNamedMember(std::string name, Type *type) {
    auto [_, succ] =
        memberIndex.insert(std::make_pair(std::move(name), memberIndex.size()));
    if (!succ) {
      return false;
    }
    addMember(type);
    return true;
  }

private:
  std::vector<CountedPtr<Type>> members;
  std::map<std::string, size_t, std::less<>> memberIndex;
};

class Symbol {
public:
  enum Kind { EMPTY, TYPE, EXTERN, STATIC, AUTO, REGISTER };
  Symbol(Kind kind, CountedPtr<Type> type)
      : kind(kind), type(std::move(type)) {}

  Kind getKind() { return kind; }

  Type &getType() { return *type; }

private:
  Kind kind;
  CountedPtr<Type> type;
};

class SymbolTable {
public:
  class Scope {
  public:
    bool declareSymbol(std::string name, Symbol symbol) {
      auto [_, succ] = identifierTypes.insert(
          std::make_pair(std::move(name), std::move(symbol)));
      return succ;
    }

    Symbol *getSymbol(std::string_view name) {
      auto it = identifierTypes.find(name);
      if (it == identifierTypes.end()) {
        return nullptr;
      }
      return &it->second;
    }

  private:
    std::map<std::string, Symbol, std::less<>> identifierTypes;
  };

  Scope &scope() { return scopes.back(); }

  void pushScope() { scopes.emplace_back(); }

  void popScope() { scopes.pop_back(); }

private:
  std::vector<Scope> scopes;
};
