#pragma once

#include "ir/Alignment.h"
#include "ir/Operand.h"

class MemoryAccessDef : public OtherSSADef {
public:
  static bool is_impl(const OtherSSADef &o) {
    return o.getKind() == LOCAL_MEMORY_ACCESS;
  }

  MemoryAccessDef(size_t size, Alignment align)
      : OtherSSADef(LOCAL_MEMORY_ACCESS), size(size), align(align) {}

  size_t getSize() { return size; }

  Alignment getAlign() { return align; }

private:
  size_t size;
  Alignment align;
};
