#pragma once

#include "ir/IRPatExecutor.h"

namespace riscv {

class PreISelCombine : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

} // namespace riscv
