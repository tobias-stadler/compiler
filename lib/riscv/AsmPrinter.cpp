#include "riscv/Arch.h"

namespace riscv {

AsmPrinterPass::AsmPrinterPass(std::ostream &out) : out(out) {}

} // namespace riscv
