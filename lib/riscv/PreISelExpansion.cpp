#include "riscv/Arch.h"

namespace {

#include "riscv/InstrSelector.dsl.preISelExpansion.h"

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
void legalizeOperand(Instr::Kind ext, Operand &op, InstrBuilder &preIr,
                     InstrBuilder &postIr) {
  if (op.isSSADef()) {
    legalizeSSADef(op, IntSSAType::get(32), postIr);
  } else if (op.isSSAUse()) {
    legalizeSSAUse(ext, op, IntSSAType::get(32), preIr);
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

void legalizePhi(PhiInstrPtr phi) {
  InstrBuilder postIr(phi->getNextNode());
  legalizeSSADef(phi->getDef(), IntSSAType::get(32), postIr);
  for (int i = 0, iEnd = phi.getNumPredecessors(); i < iEnd; ++i) {
    InstrBuilder preIr(phi.getPredecessorBlock(i).getLast());
    legalizeSSAUse(Instr::EXT_A, phi.getPredecessorUse(i), IntSSAType::get(32),
                   preIr);
  }
}
} // namespace

namespace riscv {
bool PreISelExpansion::execute(Instr &instr) {
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
} // namespace riscv
