#include "ir/IRPrinter.h"
#include "ir/IR.h"
#include "ir/IRVisitor.h"
#include <cassert>
#include <iostream>
#include <map>
#include <optional>

namespace {
class NumberingIRVisitor : public IRVisitor<NumberingIRVisitor> {
public:
  void visitFunction(Function &func) {
    ssaDefs.clear();
    nextSSADefNum = 0;
    for (auto &block : func) {
      defNum(block.getDef());
    }
    for (auto &block : func) {
      dispatch(block);
    }
  }

  void visitOperandSSADef(Operand &op) { defNum(op); }

  unsigned defNum(Operand &op) {
    auto [it, succ] = ssaDefs.insert(std::make_pair(&op, nextSSADefNum));
    if (succ) {
      return nextSSADefNum++;
    } else {
      return it->second;
    }
  }

  std::optional<unsigned> getNum(Operand &op) {
    auto it = ssaDefs.find(&op);
    if (it != ssaDefs.end()) {
      return it->second;
    }
    return std::nullopt;
  }

private:
  std::map<Operand *, unsigned> ssaDefs;
  unsigned nextSSADefNum = 0;
};

class PrintIRVisitor : public IRVisitor<PrintIRVisitor> {
public:
  void visitProgram(Program &prog) {
    for (auto &f : prog.functions) {
      std::cout << "---\n";
      dispatch(*f);
      std::cout << "---\n";
    }
  }

  void visitFunction(Function &func) {
    numbering.dispatch(func);
    for (auto &b : func) {
      std::cout << "%" << numbering.getNum(b.getDef()).value() << ":\n";
      dispatch(b);
    }
  }

  void visitBlock(Block &block) {
    for (auto &i : block) {
      dispatch(i);
      std::cout << "\n";
    }
  }

  void visitInstr(Instr &instr) {
    std::cout << Instr::kindName(instr.getKind());
    for (auto &op : instr) {
      std::cout << " ";
      dispatch(op);
    }
  }

  void visitOperand(Operand &op) {
    switch (op.getKind()) {
    case Operand::EMPTY:
      std::cout << "Empty";
      break;
    case Operand::IMM32:
      std::cout << "imm32(" << op.imm32() << ")";
      break;
    case Operand::SSA_DEF_TYPE:
      std::cout << "def(%" << numbering.defNum(op) << ",";
      printSSAType(op.ssaDefType());
      std::cout << ")";
      break;
    case Operand::SSA_DEF_REGCLASS:
      std::cout << "def(%" << numbering.defNum(op) << ",";
      std::cout << op.ssaDefRegClass() << ")";
      break;
    case Operand::SSA_USE:
      printNumberedDef(op.ssaUse().getDef());
      break;
    case Operand::SSA_DEF_BLOCK:
    case Operand::SSA_DEF_FUNCTION:
    case Operand::REG_USE:
    case Operand::REG_DEF:
      std::cout << "unnamed";
      break;
    case Operand::TYPE:
      printSSAType(op.type());
      break;
    case Operand::BLOCK:
      printNumberedDef(op.block().getDef());
      break;
    case Operand::CHAIN: {
      bool first = true;
      std::cout << "[";
      for (auto &o : op.chain()) {
        if (first) {
          first = false;
        } else {
          std::cout << " ";
        }
        dispatch(o);
      }
      std::cout << "]";
      break;
    }
    case Operand::BRCOND:
      std::cout << BrCond::kindName(op.brCond().getKind());
      break;
    }
  }

  void printNumberedDef(Operand &op) {
    if (auto n = numbering.getNum(op)) {
      std::cout << "%" << *n;
    } else {
      std::cout << "(";
      switch (op.getKind()) {
      case Operand::EMPTY:
        std::cout << "Empty";
        break;
      case Operand::SSA_DEF_TYPE:
      case Operand::SSA_DEF_REGCLASS:
        std::cout << "unnamed def";
        break;
      case Operand::SSA_DEF_BLOCK:
        std::cout << "unnamed block";
        break;
      case Operand::SSA_DEF_FUNCTION:
        std::cout << "unnamed function";
        break;
      default:
        assert(false);
        break;
      }
      std::cout << ")";
    }
  }

  void printSSAType(SSAType &type) {
    std::cout << SSAType::kindName(type.getKind());
    if (type.getKind() == SSAType::INT) {
      std::cout << static_cast<IntSSAType &>(type).getBits();
    }
  }

  NumberingIRVisitor numbering;
};

} // namespace

template <class T> void PrintIR(T &ir) { PrintIRVisitor().dispatch(ir); }

template void PrintIR<Instr>(Instr &);
template void PrintIR<Block>(Block &);
template void PrintIR<Function>(Function &);
template void PrintIR<Program>(Program &);
