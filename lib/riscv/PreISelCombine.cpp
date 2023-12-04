#include "riscv/Arch.h"

namespace {

#include "riscv/InstrSelector.dsl.preISelCombine.h"

}

namespace riscv {

bool PreISelCombine::execute(Instr &instr) {
  if (!dslExecutePat(instr)) {
    return false;
  }
  instr.deleteThis();
  return true;
}

} // namespace riscv
