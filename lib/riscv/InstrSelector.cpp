#include "ir/InstrSelector.h"
#include "ir/IR.h"
#include "riscv/Arch.h"
#include "support/TCInt.h"

namespace {

constexpr unsigned XLEN = 32;

unsigned selectExtShiftBits(Operand &op) {
  return XLEN - static_cast<IntSSAType &>(op.ssaDefType()).getBits();
}

#include "riscv/InstrSelector.dsl.isel.h"
} // namespace

namespace riscv {

bool InstrSelect::execute(Instr &instr) {
  if (!dslExecutePat(instr)) {
    return false;
  }
  instr.deleteThis();
  return true;
}

} // namespace riscv
