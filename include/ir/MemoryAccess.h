#pragma once


#include "ir/Operand.h"

class MemoryAccessDef : public OtherSSADef {
  public:

    MemoryAccessDef() :OtherSSADef(LOCAL_MEMORY_ACCESS) {

    }
};
