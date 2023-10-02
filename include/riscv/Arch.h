#pragma once

#include "ir/IR.h"
#include "ir/InstrSelector.h"

namespace riscv {

#include "riscv/Arch.dsl.h"

class InstrSelector : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

} // namespace riscv
