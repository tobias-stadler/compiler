#include <frontend/Type.h>

PtrType *Type::getDerivedPtr() {
  if (derivedPtr) {
    return derivedPtr;
  }
  derivedPtr = new PtrType(this);
  return derivedPtr;
}
