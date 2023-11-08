#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRPatExecutor.h"
#include "ir/IRVisitor.h"
#include "ir/InstrBuilder.h"
#include <cassert>
#include <initializer_list>

namespace riscv {

#include "riscv/Arch.dsl.riscv.h"

class Arch : public ::Arch {
public:
  const char *getInstrKindName(unsigned kind) override {
    return riscv::instrKindName(kind);
  }
  const char *getRegisterKindName(unsigned kind) override {
    return riscv::registerKindName(kind);
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

class BranchLoweringPass : public IRPass<Function>,
                           public IRVisitor<BranchLoweringPass> {
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
    }
  }

  void lowerCondBr(Instr &i) {
    Instr *newBr = new Instr(i.getKind());
    newBr->allocateOperands(3);
    newBr->emplaceOperand<Operand::SSA_USE>(i.getOperand(0).ssaUse().getDef());
    newBr->emplaceOperand<Operand::SSA_USE>(i.getOperand(1).ssaUse().getDef());
    newBr->emplaceOperand<Operand::SSA_USE>(i.getOperand(2).ssaUse().getDef());
    Instr *newJmp = new Instr(JAL);
    newJmp->allocateOperands(2);
    newJmp->emplaceOperand<Operand::REG_USE>(X0);
    newJmp->emplaceOperand<Operand::SSA_USE>(i.getOperand(3).ssaUse().getDef());
    i.insertPrev(newBr);
    i.insertPrev(newJmp);
    i.deleteThis();
  }

  const char *name() override { return "RiscVBranchLowering"; }
};

class AsmPrinter : public IRPass<Program> {
  const char *name() override { return "RiscVAsmPrinter"; }
  void run(Program &obj, IRInfo<Program> &info) override {}

  void printFunction(Function &func) {}

  void printInstr(Instr &instr) {
    switch (instr.getKind()) {
    default:
      assert(false && "Unsupported instruction");
      break;
    case AUIPC:
    case JAL:
    case JALR:
    case BEQ:
    case BNE:
    case BLT:
    case BGE:
    case BLTU:
    case BGEU:
    case SLTI:
    case SLTIU:
    case SLT:
    case SLTU:
    case ADDI:
    case ORI:
    case ANDI:
    case XORI:
    case SLLI:
    case SRLI:
    case SRAI:
    case ADD:
    case SUB:
    case OR:
    case AND:
    case XOR:
    case SLL:
    case SRL:
    case SRA:
    case LB:
    case LH:
    case LW:
    case LBU:
    case LHU:
    case SB:
    case SH:
    case SW:
      break;
    }
  }

  void printOperand() {}

  std::ostream out;
};

} // namespace riscv
