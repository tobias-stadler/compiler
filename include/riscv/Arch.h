#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRPatExecutor.h"
#include "ir/IRVisitor.h"
#include "ir/InstrBuilder.h"
#include <array>
#include <cassert>
#include <initializer_list>

namespace riscv {

#include "riscv/Arch.dsl.riscv.h"

class Arch : public ::Arch {
public:
  std::span<const ArchReg> getArchRegs() override { return archRegs; };
  const ArchReg *getArchReg(unsigned kind) override {
    return riscv::getArchReg(kind);
  }
  const ArchInstr *getArchInstr(unsigned kind) override {
    return riscv::getArchInstr(kind);
  }
  const ArchRegClass *getArchRegClass(unsigned kind) override {
    return riscv::getArchRegClass(kind);
  }
};

class InstrSelect : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class PreISelExpansion : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class PreISelCombine : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class PostRALowering : public IRPass<Function>,
                       public IRVisitor<PostRALowering> {
public:
  void run(Function &func, IRInfo<Function> &info) override { dispatch(func); }

  void visitInstr(Instr &instr) {
    switch (instr.getKind()) {
    case BEQ:
    case BNE:
    case BLT:
    case BLTU:
    case BGE:
    case BGEU:
      lowerCondBr(instr);
      break;
    case Instr::COPY:
      lowerCopy(instr);
      break;
    }
  }

  void lowerCopy(Instr &i) {
    if (i.getOperand(0).reg() == i.getOperand(1).reg()) {
      i.deleteThis();
      return;
    }
    Instr *newI = new Instr(ADDI);
    newI->allocateOperands(3);
    newI->emplaceOperand<Operand::REG_DEF>(i.getOperand(0).reg());
    newI->emplaceOperand<Operand::REG_USE>(i.getOperand(1).reg());
    newI->emplaceOperand<Operand::IMM32>(0);
    i.insertPrev(newI);
    i.deleteThis();
  }

  void lowerCondBr(Instr &i) {
    Instr *newBr = new Instr(i.getKind());
    newBr->allocateOperands(3);
    newBr->addOperand(i.getOperand(0));
    newBr->addOperand(i.getOperand(1));
    newBr->emplaceOperand<Operand::SSA_USE>(i.getOperand(2).ssaUse().getDef());
    Instr *newJmp = new Instr(JAL);
    newJmp->allocateOperands(2);
    newJmp->emplaceOperand<Operand::REG_USE>(X0);
    newJmp->addOperand(i.getOperand(3));
    i.insertPrev(newBr);
    i.insertPrev(newJmp);
    i.deleteThis();
  }

  const char *name() override { return "RiscVPostRALoweringPass"; }
};

class AsmPrinterPass : public IRPass<Program> {
public:
  AsmPrinterPass(std::ostream &out) : out(out) {}
  const char *name() override { return "RiscVAsmPrinterPass"; }
  void run(Program &prog, IRInfo<Program> &info) override {
    out << ".option nopic\n";
    out << ".attribute arch, \"rv32i\"\n";
    out << ".text\n";
    for (auto &func : prog.functions) {
      printFunction(*func);
    }
  }

  void printFunction(Function &func) {
    blockToNum.clear();
    unsigned blockNum = 0;
    for (auto &block : func) {
      blockToNum[&block] = blockNum++;
    }
    printFunctionLabel(func);
    out << ":\n";
    for (auto &block : func) {
      printBlock(block);
    }
  }

  void printBlock(Block &block) {
    printBlockLabel(block);
    out << ":\n";
    currIndent = 1;
    for (auto &instr : block) {
      printIndent();
      printInstr(instr);
      out << "\n";
    }
  }

  void printInstr(Instr &instr) {
    const ArchInstr *archInstr = getArchInstr(instr.getKind());
    if (!archInstr || !archInstr->alias) {
      assert(false && "Illegal instruction");
      return;
    }
    out << archInstr->alias;
    if (instr.getNumOperands() != 0) {
      out << " ";
    }
    bool first = true;
    for (auto &op : instr) {
      if (first) {
        first = false;
      } else {
        out << ", ";
      }
      printOperand(op);
    }
  }

  void printFunctionLabel(Function &func) { out << func.name; }

  void printBlockLabel(Block &block) { out << ".Lbb" << blockToNum[&block]; }

  void printOperand(Operand &op) {
    switch (op.getKind()) {
    case Operand::EMPTY:
      break;
    case Operand::SSA_DEF_TYPE:
    case Operand::SSA_DEF_BLOCK:
    case Operand::SSA_DEF_FUNCTION:
    case Operand::TYPE:
    case Operand::BLOCK:
    case Operand::BRCOND:
    case Operand::CHAIN:
      assert(false && "Illegal operand");
      break;
    case Operand::SSA_USE: {
      if (op.ssaUse().getDef().getKind() != Operand::SSA_DEF_BLOCK) {
        assert(false && "Illegal operand");
        return;
      }
      printBlockLabel(op.ssaUse().getDef().ssaDefBlock());
      break;
    }
    case Operand::REG_DEF:
    case Operand::REG_USE: {
      const ArchReg *archReg = getArchReg(op.reg());
      if (!archReg || !archReg->alias) {
        assert(false && "Illegal register");
        return;
      }
      out << archReg->alias;
      break;
    }
    case Operand::IMM32: {
      out << op.imm32();
      break;
    }
    }
  }

  void printIndent() {
    for (unsigned i = 0; i < currIndent; ++i) {
      out << "  ";
    }
  }

  unsigned currIndent;
  std::unordered_map<Block *, unsigned> blockToNum;
  std::ostream &out;
};

} // namespace riscv
