#pragma once

#include "c/Type.h"
#include "ir/Alignment.h"
#include "support/RTTI.h"
#include "support/Utility.h"

namespace c {

class StructLayout {
public:
  size_t size;
  Alignment align;
  std::vector<size_t> offsets;
};

class MemoryLayout {
public:
  std::pair<size_t, Alignment> getSizeAndAlignment(Type &type) {
    switch (type.getKind()) {
    case Type::BOOL:
    case Type::SCHAR:
    case Type::UCHAR:
      return {1, Alignment(0)};
    case Type::SSHORT:
    case Type::USHORT:
      return {2, Alignment(1)};
    case Type::SINT:
    case Type::UINT:
      return {4, Alignment(2)};
    case Type::ULONG:
    case Type::SLONG:
    case Type::SLONGLONG:
    case Type::ULONGLONG:
      return {4, Alignment(2)};
    case Type::UNION:
    case Type::STRUCT: {
      auto &structLayout = getStructLayout(as<StructType>(type));
      return {structLayout.size, structLayout.align};
    }
    case Type::PTR:
      return {4, Alignment(2)};
    case Type::QUALIFIED:
      return getSizeAndAlignment(as<QualifiedType>(type).getBaseType());
    case Type::ENUM:
      return getSizeAndAlignment(as<EnumType>(type).getBaseType());
    default:
      UNREACHABLE("Invalid type");
      break;
    }
  }

  StructLayout &getStructLayout(StructType &type) {
    auto [it, succ] = structLayoutCache.try_emplace(&type);
    auto &structLayout = it->second;
    if (!succ) {
      return structLayout;
    }

    size_t offset = 0;
    Alignment align(0);
    for (auto &m : type.members) {
      auto [mSize, mAlign] = getSizeAndAlignment(*m);
      align = Alignment(std::max(align.getExp(), mAlign.getExp()));
      if (type.isUnion()) {
        offset = std::max(offset, mSize);
      } else {
        offset = mAlign.alignSize(offset);
        structLayout.offsets.push_back(offset);
        offset += mSize;
      }
    }
    offset = align.alignSize(offset);

    structLayout.size = offset;
    structLayout.align = align;
    return structLayout;
  }

  void clearCache() { structLayoutCache.clear(); }

private:
  std::unordered_map<Type *, StructLayout> structLayoutCache;
};
} // namespace c
