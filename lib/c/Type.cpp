#include "c/Type.h"
#include "support/RefCount.h"
#include <cassert>

namespace {
auto initBasicTypes(bool c) {
  std::array<BasicType, Type::NUM_BASIC> arr;
  for (int i = 0, kind = Type::BASIC_START + 1; kind < Type::BASIC_END;
       ++kind, ++i) {
    arr[i] = BasicType(static_cast<Type::Kind>(kind), Type::Qualifier(c));
    arr[i].incRefCount();
  }
  return arr;
}
std::array<BasicType, Type::NUM_BASIC> basicTypes = initBasicTypes(false);
std::array<BasicType, Type::NUM_BASIC> basicTypesC = initBasicTypes(true);
} // namespace

BasicType::BasicType(Kind kind, Qualifier qualifier) : Type(kind, qualifier) {
  assert(isBasic(kind) && "Expected basic kind");
}

CountedPtr<BasicType> BasicType::create(Kind kind, Qualifier qualifier) {
  assert(isBasic(kind) && "Expected basic kind");
  if (qualifier.onlyConst()) {
    int idx = kind - BASIC_START - 1;
    if (qualifier.isConst()) {
      return CountedPtr(&basicTypesC[idx]);
    } else {
      return CountedPtr(&basicTypes[idx]);
    }
  }
  return make_counted<BasicType>(kind, qualifier);
}

bool operator==(const Type &a, const Type &b) {
  if (a.kind != b.kind) {
    return false;
  }
  if (Type::isBasic(a.kind)) {
    return static_cast<const BasicType &>(a) ==
           static_cast<const BasicType &>(b);
  }
  if (a.kind == Type::PTR) {
    return static_cast<const PtrType &>(a) == static_cast<const PtrType &>(b);
  }

  return false;
}
