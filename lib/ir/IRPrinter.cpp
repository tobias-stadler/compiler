#include "ir/IRPrinter.h"
#include "ir/IR.h"
#include "ir/IRVisitor.h"
#include <cassert>
#include <iostream>
#include <map>
#include <optional>

const IRInfoID NumberingIRVisitor::ID = nullptr;
const IRInfoID PrintIRVisitor::ID = nullptr;

void PrintIRVisitor::printNumberedDef(Operand &op) {
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

void PrintIRVisitor::printSSAType(SSAType &type) {
  std::cout << SSAType::kindName(type.getKind());
  if (type.getKind() == SSAType::INT) {
    std::cout << static_cast<IntSSAType &>(type).getBits();
  }
}

void PrintIRVisitor::printReg(Reg reg) {
  if (arch) {
    const ArchReg *archReg = arch->getArchReg(reg);
    if (archReg) {
      std::cout << archReg->name;
      return;
    }
  }
  std::cout << "@" << reg.getNum();
}

void PrintIRVisitor::printRegClass(unsigned kind) {
  if (arch) {
    const ArchRegClass *archRegClass = arch->getArchRegClass(kind);
    if (archRegClass) {
      std::cout << archRegClass->name;
      return;
    }
  }
  std::cout << "RC" << kind;
}

void PrintIRVisitor::visitProgram(Program &prog) {
  for (auto &f : prog.functions) {
    std::cout << "---\n";
    dispatch(*f);
    std::cout << "---\n";
  }
}

void PrintIRVisitor::visitFunction(Function &func) {
  numbering.dispatch(func);
  for (auto &b : func) {
    std::cout << "%" << numbering.getNum(b.getDef()).value() << ":\n";
    dispatch(b);
  }
}

void PrintIRVisitor::visitBlock(Block &block) {
  for (auto &i : block) {
    dispatch(i);
    std::cout << "\n";
  }
}

void PrintIRVisitor::visitInstr(Instr &instr) {
  preInstrCallback(instr);
  const ArchInstr *archInstr =
      arch ? arch->getArchInstr(instr.getKind()) : nullptr;
  const char *name =
      archInstr ? archInstr->name : Instr::kindName(instr.getKind());
  std::cout << name;
  for (auto &op : instr) {
    std::cout << " ";
    dispatch(op);
  }
}

void PrintIRVisitor::visitOperand(Operand &op) {
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
    printRegClass(op.ssaDefRegClass());
    std::cout << ")";
    break;
  case Operand::SSA_USE:
    printNumberedDef(op.ssaUse().getDef());
    break;
  case Operand::SSA_DEF_BLOCK:
  case Operand::SSA_DEF_FUNCTION:
    std::cout << "unnamed";
    break;
  case Operand::REG_USE:
    printReg(op.reg());
    break;
  case Operand::REG_DEF:
    std::cout << "def(";
    printReg(op.reg());
    std::cout << ")";
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

void NumberingIRVisitor::visitFunction(Function &func) {
  for (auto &block : func) {
    defNum(block.getDef());
  }
  for (auto &block : func) {
    dispatch(block);
  }
}

void NumberingIRVisitor::visitOperandSSADef(Operand &op) { defNum(op); }

unsigned NumberingIRVisitor::defNum(Operand &op) {
  auto [it, succ] = ssaDefs.insert(std::make_pair(&op, nextSSADefNum));
  if (succ) {
    ++nextSSADefNum;
  }
  return it->second;
}

std::optional<unsigned> NumberingIRVisitor::getNum(Operand &op) {
  auto it = ssaDefs.find(&op);
  if (it != ssaDefs.end()) {
    return it->second;
  }
  return std::nullopt;
}
