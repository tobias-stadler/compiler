#pragma once

#include "ir/IRPatExecutor.h"

namespace riscv {

class InstrSelect : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

} // namespace riscv
