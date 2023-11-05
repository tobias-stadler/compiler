#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRVisitor.h"
#include "ir/InstrBuilder.h"
#include "ir/InstrSelector.h"
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
#include "riscv/InstrSelector.dsl.preISelExpansion.h"
  bool execute(Instr &instr) override {
    switch (instr.getKind()) {
    case Instr::PHI:
      legalizePhi(PhiInstrPtr(&instr));
      break;
    case Instr::CONST_INT:
    case Instr::SL_L:
    case Instr::SR_L:
    case Instr::SR_A:
      legalizeAllOperands(Instr::EXT_Z, instr);
      break;
    case Instr::AND:
    case Instr::OR:
    case Instr::XOR:
    case Instr::ADD:
    case Instr::SUB:
      legalizeAllOperands(Instr::EXT_A, instr);
      break;
    case Instr::CMP:
      legalizeOperands(Instr::EXT_Z, instr,
                       {&instr.getOperand(2), &instr.getOperand(3)});
      break;
    }
    dslExecutePat(instr);
    return true;
  }

  void legalizePhi(PhiInstrPtr phi) {
    InstrBuilder postIr(phi->getNextNode());
    legalizeSSADef(phi->getDef(), IntSSAType::get(32), postIr);
    for (int i = 0, iEnd = phi.getNumPredecessors(); i < iEnd; ++i) {
      InstrBuilder preIr(phi.getPredecessorBlock(i).getLast());
      legalizeSSAUse(Instr::EXT_A, phi.getPredecessorUse(i),
                     IntSSAType::get(32), preIr);
    }
  }

  void legalizeOperands(Instr::Kind ext, Instr &instr,
                        std::initializer_list<Operand *> ops) {
    InstrBuilder preIr(instr);
    InstrBuilder postIr(instr.getNextNode());
    for (auto *op : ops) {
      legalizeOperand(ext, *op, preIr, postIr);
    }
  }

  void legalizeAllOperands(Instr::Kind ext, Instr &instr) {
    InstrBuilder preIr(instr);
    InstrBuilder postIr(instr.getNextNode());
    for (auto &op : instr) {
      legalizeOperand(ext, op, preIr, postIr);
    }
  }

  void legalizeOperand(Instr::Kind ext, Operand &op, InstrBuilder &preIr,
                       InstrBuilder &postIr) {
    if (op.isSSADef()) {
      legalizeSSADef(op, IntSSAType::get(32), postIr);
    } else if (op.isSSAUse()) {
      legalizeSSAUse(ext, op, IntSSAType::get(32), preIr);
    }
  }

  void legalizeSSAUse(Instr::Kind ext, Operand &op, IntSSAType &expectedTy,
                      InstrBuilder &ir) {
    assert(op.isSSAUse());
    Operand &def = op.ssaUse().getDef();
    if (!def.isSSARegDef() || def.ssaDefType().getKind() != SSAType::INT) {
      return;
    }
    IntSSAType &intTy = static_cast<IntSSAType &>(def.ssaDefType());
    if (intTy.getBits() < expectedTy.getBits()) {
      ir.emitExt(ext, expectedTy, op.ssaUse().getDef());
      op.ssaUseReplace(ir.getDef());
    }
  }

  void legalizeSSADef(Operand &op, IntSSAType &expectedTy, InstrBuilder &ir) {
    assert(op.isSSADef());
    SSAType &ty = op.ssaDefType();
    if (ty.getKind() != SSAType::INT) {
      return;
    }
    IntSSAType &intTy = static_cast<IntSSAType &>(ty);
    if (intTy.getBits() < expectedTy.getBits()) {
      op.ssaDefSetType(expectedTy);
      ir.emitTrunc(intTy, op);
      op.ssaDef().replaceAllUses(ir.getDef());
      ir.getLastInstr()->getOperand(1).ssaUseReplace(op);
    }
  }
};

class PreISelCombine : public IRPatExecutor {
#include "riscv/InstrSelector.dsl.preISelCombine.h"
  bool execute(Instr &instr) override {
    if (!dslExecutePat(instr)) {
      return false;
    }
    instr.deleteThis();
    return false;
  }
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
