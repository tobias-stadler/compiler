#pragma once
#include "ir/IR.h"
#include "support/RefCount.h"
#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

class Type : public IntrusiveCountedPtrBase {
public:
  enum Kind {
    EMPTY,
    BASIC_START,
    VOID,
    INTEGER_START,
    BOOL,
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
    INTEGER_END,
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

  static constexpr bool isBasic(Kind kind) {
    return kind > BASIC_START && kind < BASIC_END;
  }

  static constexpr bool isDerived(Kind kind) {
    return kind > DERIVED_START && kind < DERIVED_END;
  }

  static constexpr bool isInteger(Kind kind) {
    return kind > INTEGER_START && kind < INTEGER_END;
  }

  static bool isSigned(Kind kind) {
    switch (kind) {
    case SCHAR:
    case SSHORT:
    case SINT:
    case SLONG:
    case SLONGLONG:
      return true;
    default:
      return false;
    }
  }

  static SSAType &irType(Type::Kind kind) {
    switch (kind) {
    case Type::BOOL:
      return IntSSAType::get(1);
    case Type::SCHAR:
    case Type::UCHAR:
      return IntSSAType::get(8);
    case Type::SSHORT:
    case Type::USHORT:
      return IntSSAType::get(16);
    case Type::SINT:
    case Type::UINT:
      return IntSSAType::get(32);
    case Type::PTR:
      return PtrSSAType::get();
    default:
      assert(false && "Unsupported type for direct IR conversion");
      __builtin_unreachable();
    }
  }

  static constexpr int NUM_BASIC = BASIC_END - BASIC_START - 1;

protected:
  Type(Kind kind, Qualifier qualifier) : kind(kind), qualifier(qualifier) {}

private:
  Kind kind;
  Qualifier qualifier;
};

class DerivedType : public Type {
protected:
  DerivedType(Kind kind, CountedPtr<Type> baseType, Qualifier qualifier)
      : Type(kind, qualifier), baseType(baseType) {}

public:
  CountedPtr<Type> &getBaseType() { return baseType; }

  void setBaseType(CountedPtr<Type> newBase) { baseType = std::move(newBase); }

private:
  CountedPtr<Type> baseType;
};

class BasicType : public Type {
public:
  BasicType(Kind kind = VOID, Qualifier qualifier = Qualifier());
  static CountedPtr<BasicType> create(Kind kind,
                                      Qualifier qualifier = Qualifier());
};

class PtrType : public DerivedType {
public:
  PtrType(CountedPtr<Type> baseType = nullptr,
          Qualifier qualifier = Qualifier())
      : DerivedType(PTR, std::move(baseType), qualifier) {}
};

class ArrType : public DerivedType {
public:
  ArrType() : DerivedType(ARR, nullptr, Qualifier()) {}
};

class FuncType : public DerivedType {
public:
  FuncType() : DerivedType(FUNC, nullptr, Qualifier()) {}
  CountedPtr<Type> &getParamType(size_t i) { return paramTypes[i]; }
  size_t getNumParams() { return paramTypes.size(); };
  void addParam(CountedPtr<Type> type) {
    paramTypes.push_back(std::move(type));
  }

  bool addNamedParam(std::string_view name, CountedPtr<Type> type) {
    auto [_, succ] = paramIndex.emplace(name, paramTypes.size());
    if (!succ) {
      return false;
    }
    addParam(std::move(type));
    return true;
  }

  Type *getNamedParamType(std::string_view name) {
    auto it = paramIndex.find(name);
    if (it == paramIndex.end()) {
      return nullptr;
    }
    return paramTypes[it->second].get();
  }

  auto &getParamTypes() { return paramTypes; }
  auto &getParamIndex() { return paramIndex; }

private:
  std::vector<CountedPtr<Type>> paramTypes;
  std::unordered_map<std::string_view, size_t> paramIndex;
};

class StructType : public Type {
public:
  StructType() : Type(STRUCT, Qualifier()) {}

  Type *getNamedMemberType(std::string_view name) {
    auto it = memberIndex.find(name);
    if (it == memberIndex.end()) {
      return nullptr;
    }
    return members[it->second].get();
  }

  void addMember(Type *type) { members.push_back(type); }

  bool addNamedMember(std::string_view name, Type *type) {
    auto [_, succ] = memberIndex.emplace(name, members.size());
    if (!succ) {
      return false;
    }
    addMember(type);
    return true;
  }

private:
  std::vector<CountedPtr<Type>> members;
  std::unordered_map<std::string_view, size_t> memberIndex;
};
