#include "c/Type.h"
#include "support/RefCount.h"
#include <cassert>

namespace c {

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

Type &Type::unqualified() {
  if (auto *qualifiedTy = as_dyn<QualifiedType>(*this)) {
    return qualifiedTy->getBaseType();
  }
  return *this;
}
} // namespace c
