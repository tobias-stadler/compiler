#pragma once

#include "ir/Alignment.h"
#include "ir/Operand.h"

class MemoryAccessDef : public ExternSSADef {
public:
  static bool is_impl(const ExternSSADef &o) {
    return o.getKind() == FUNC_MEMORY_ACCESS;
  }

  MemoryAccessDef(size_t size, Alignment align)
      : ExternSSADef(FUNC_MEMORY_ACCESS), size(size), align(align) {}

  size_t getSize() { return size; }

  Alignment getAlign() { return align; }

private:
  size_t size;
  Alignment align;
};
