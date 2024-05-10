#include "riscv/InstrSelect.h"
#include "ir/IR.h"
#include "riscv/Arch.h"
#include "support/MachineInt.h"

namespace {

unsigned selectExtShiftBits(Operand &op) {
  return riscv::XLEN - static_cast<IntSSAType &>(op.ssaDef().type()).getBits();
}

bool isLegalImm(Operand &op) { return op.mInt().canSTrunc(12); }
bool isLegalImmNegated(Operand &op) { return (-op.mInt()).canSTrunc(12); }
bool isLegalMemAccess(Operand &op, size_t alignExp) {
  MemoryAccessDef &mem = op.ssaDefExtern<MemoryAccessDef>();
  return mem.getSize() == (1UL << alignExp) &&
         mem.getAlign().getExp() == alignExp;
}

#include "riscv/InstrSelector.dsl.isel.h"

} // namespace

namespace riscv {

bool InstrSelect::execute(Instr &instr) {
  if (DslPatExecutor(observer).dslExecutePat(instr)) {
    instr.deleteThis();
    return true;
  }
  if (instr.getKind() == Instr::REF_EXTERN)
    return true;

  return false;
}

} // namespace riscv
