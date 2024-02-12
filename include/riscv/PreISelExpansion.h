#pragma once

#include "ir/IRPatExecutor.h"

namespace riscv {

class PreISelExpansion : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

} // namespace riscv
