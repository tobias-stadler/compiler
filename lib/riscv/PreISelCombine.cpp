#include "riscv/PreISelCombine.h"
#include "riscv/Arch.h"

namespace {

#include "riscv/InstrSelector.dsl.preISelCombine.h"

}

namespace riscv {

bool PreISelCombine::execute(Instr &instr) {
  if (!DslPatExecutor(observer).dslExecutePat(instr)) {
    return false;
  }
  instr.deleteThis();
  return true;
}

} // namespace riscv
