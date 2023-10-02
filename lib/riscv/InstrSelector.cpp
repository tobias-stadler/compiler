#include "ir/InstrSelector.h"
#include "riscv/Arch.h"
#include "support/TCInt.h"

namespace {
#include "riscv/InstrSelector.dsl.h"
}

namespace riscv {

bool InstrSelector::execute(Instr &instr) { return dslSelectInstr(instr); }

} // namespace riscv
