#include "riscv/InstrSelect.h"
#include "ir/IR.h"
#include "riscv/Arch.h"
#include "support/TCInt.h"

namespace {

unsigned selectExtShiftBits(Operand &op) {
  return riscv::XLEN - static_cast<IntSSAType &>(op.ssaDefType()).getBits();
}

bool isLegalImm(Operand &op) {
  int32_t imm = op.imm32();
  return TCInt::canTrunc<12>(imm);
}
bool isLegalImmNegated(Operand &op) {
  int32_t imm = op.imm32();
  return imm > INT32_MIN && TCInt::canTrunc<12>(-imm);
}
bool isLegalMemAccess(Operand& op, size_t alignExp) {
  MemoryAccessDef& mem = as<MemoryAccessDef>(op.ssaDefOther());
  return mem.getSize() == (1UL << alignExp) && mem.getAlign().getExp() == alignExp;
}

#include "riscv/InstrSelector.dsl.isel.h"

} // namespace

namespace riscv {

bool InstrSelect::execute(Instr &instr) {
  if (DslPatExecutor(observer).dslExecutePat(instr)) {
    instr.deleteThis();
    return true;
  }
  if (instr.getKind() == Instr::REF_OTHERSSADEF)
    return true;

  return false;
}

} // namespace riscv
