#pragma once
#include <array>
#include <functional>
#include <map>
#include <string>
#include <utility>
#include <vector>
class PtrType;

class Type {
public:
  enum Kind { I1, I8, I16, I32, I64, PTR, STRUCT, FUNC };
  Kind kind;

  Type(Kind kind) : kind(kind) {}
  PtrType *getDerivedPtr();

private:
  PtrType *derivedPtr;
};

class PtrType : public Type {
public:
  PtrType(Type *subType) : Type(PTR), subType(subType) {}

  Type *getSubType() { return subType; }

private:
  Type *subType;
};

class ArrType : public Type {
  Type *getSubType() { return elementType; }

private:
  Type *elementType;
};

class FuncType : public Type {
public:
  Type *getRetType() { return retType; }
  Type *gerParamType(size_t i) { return paramTypes[i]; }
  size_t getNumParams() { return paramTypes.size(); };

private:
  Type *retType;
  std::vector<Type *> paramTypes;
};

class StructType : public Type {
public:
  Type *getMemberType(std::string_view v) {
    auto it = memberTypes.find(v);
    if (it == memberTypes.end()) {
      return nullptr;
    }
    return it->second;
  }

  bool addMember(std::string name, Type *type) {
    return memberTypes.insert(std::make_pair(std::move(name), type)).second;
  }

private:
  std::map<std::string, Type *, std::less<>> memberTypes;
};

class TypeTable {
public:
  Type *getChar() { return &primitives[0]; }
  Type *getShort() { return &primitives[1]; }
  Type *getInt() { return &primitives[2]; }
  Type *getLong() { return &primitives[2]; }

private:
  std::map<std::string, Type *> typeNames;
  std::map<std::string, FuncType *> functions;
  std::array<Type, 3> primitives = {Type(Type::I8), Type(Type::I16),
                                    Type(Type::I32)};
};
