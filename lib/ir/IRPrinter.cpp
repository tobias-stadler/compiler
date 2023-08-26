#include "ir/IRPrinter.h"
#include "ir/IR.h"
#include "ir/IRVisitor.h"
#include <iostream>
#include <map>

namespace {
class PrintIRVisitor : public IRVisitor<PrintIRVisitor> {
public:
  void visitBlock(Block &block) {
    for (auto &i : block) {
      visitInstr(i);
      std::cout << "\n";
    }
  }

  void visitInstr(Instr &instr) {
    std::cout << Instr::kindName(instr.getKind());
    for (auto &op : instr) {
      std::cout << " ";
      switch (op.getKind()) {
      case Operand::EMPTY:
        continue;
      case Operand::IMM32:
        std::cout << op.imm32();
        break;
      case Operand::SSA_DEF_TYPE:
      case Operand::SSA_DEF_REGCLASS:
        std::cout << "def(%" << defSSA(op) << ")";
        break;
      case Operand::SSA_USE:
        printSSAUse(op);
        break;
      default:
        std::cout << "?";
      }
    }
  }

private:
  std::map<SSADef *, unsigned> ssaDefs;
  unsigned nextSSADefNum = 0;
  unsigned defSSA(Operand &op) {
    unsigned num = nextSSADefNum++;
    ssaDefs.insert(std::make_pair(&op.ssaDef(), num));
    return num;
  }
  void printSSAUse(Operand &op) {
    if (auto it = ssaDefs.find(&op.ssaUse().getDef()); it != ssaDefs.end()) {
      std::cout << "%" << it->second;
    } else {
      std::cout << "(";
      visitInstr(op.ssaUse().getDef().getParent());
      std::cout << ")";
    }
  }
};

} // namespace

template <class T> void PrintIR(T &ir) { PrintIRVisitor().dispatch(ir); }

template void PrintIR<Instr>(Instr &);
template void PrintIR<Block>(Block &);
template void PrintIR<Function>(Function &);
