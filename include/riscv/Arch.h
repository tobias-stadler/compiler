#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include "ir/InstrSelector.h"

namespace riscv {

#include "riscv/Arch.dsl.h"

class InstrSelector : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class Arch : public ::Arch {
public:
  const char *getInstrKindName(unsigned kind) override {
    return riscv::instrKindName(kind);
  }
};

} // namespace riscv
