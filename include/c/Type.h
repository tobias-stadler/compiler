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
    UNION,
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
    bool isConst() const { return qConst; }
    bool isVolatile() const { return qVolatile; }
    bool isRestrict() const { return qRestrict; }
    bool isAtomic() const { return qAtomic; }
    bool onlyConst() const { return !(qRestrict || qVolatile || qAtomic); }

  private:
    bool qConst, qVolatile, qRestrict, qAtomic;
  };
  virtual ~Type() = default;

  Kind getKind() const { return kind; }

  const Qualifier &getQualifier() const { return qualifier; }

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

  static constexpr int NUM_BASIC = BASIC_END - BASIC_START - 1;

  friend bool operator==(const Type &a, const Type &b);

  friend bool operator!=(const Type &a, const Type &b) { return !(a == b); }

protected:
  Type(Kind kind, Qualifier qualifier) : kind(kind), qualifier(qualifier) {}
  Qualifier qualifier;

private:
  Kind kind;
};

class DerivedType : public Type {
protected:
  DerivedType(Kind kind, CountedPtr<Type> baseType, Qualifier qualifier)
      : Type(kind, qualifier), baseType(baseType) {}

public:
  CountedPtr<Type> &getBaseType() { return baseType; }

  void setBaseType(CountedPtr<Type> newBase) { baseType = std::move(newBase); }

  friend bool operator==(const DerivedType &a, const DerivedType &b) {
    if (a.getKind() != b.getKind()) {
      return false;
    }
    if (bool(a.baseType) != bool(b.baseType)) {
      return false;
    }
    if (!bool(a.baseType)) {
      return true;
    }

    if (*a.baseType == *b.baseType) {
      return true;
    }

    return false;
  }

private:
  CountedPtr<Type> baseType;
};

class BasicType : public Type {
public:
  BasicType(Kind kind = VOID, Qualifier qualifier = Qualifier());
  static CountedPtr<BasicType> create(Kind kind,
                                      Qualifier qualifier = Qualifier());

  friend bool operator==(const BasicType &a, const BasicType &b) {
    return a.getKind() == b.getKind();
  }
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

  CountedPtr<Type> &getParamType(size_t i) { return params[i].second; }

  std::string_view getParamName(size_t i) { return params[i].first; }

  size_t getNumParams() { return params.size(); };

  void addParam(CountedPtr<Type> type) {
    params.emplace_back(std::string_view(), std::move(type));
  }

  bool addNamedParam(std::string_view name, CountedPtr<Type> type) {
    auto [_, succ] = paramIndex.try_emplace(name, params.size());
    if (!succ) {
      return false;
    }
    params.emplace_back(name, std::move(type));
    return true;
  }

  Type *getNamedParamType(std::string_view name) {
    auto it = paramIndex.find(name);
    if (it == paramIndex.end()) {
      return nullptr;
    }
    return params[it->second].second.get();
  }

  const auto &getParams() { return params; }
  const auto &getParamIndex() { return paramIndex; }

private:
  std::vector<std::pair<std::string_view, CountedPtr<Type>>> params;
  std::unordered_map<std::string_view, size_t> paramIndex;
};

class StructType : public Type {
public:
  StructType(bool isUnion, std::string_view name)
      : Type(isUnion ? UNION : STRUCT, Qualifier()), name(name) {}

  Type *getNamedMemberType(std::string_view name) {
    auto it = memberIndex.find(name);
    if (it == memberIndex.end()) {
      return nullptr;
    }
    return members[it->second].get();
  }

  void addMember(CountedPtr<Type> type) {
    members.emplace_back(std::move(type));
  }

  bool addNamedMember(std::string_view name, CountedPtr<Type> type) {
    auto [_, succ] = memberIndex.try_emplace(name, members.size());
    if (!succ) {
      return false;
    }
    addMember(type);
    return true;
  }

  void setQualifier(Qualifier quali) { qualifier = quali; }

  std::vector<CountedPtr<Type>> members;

private:
  std::string_view name;
  std::unordered_map<std::string_view, size_t> memberIndex;
};
