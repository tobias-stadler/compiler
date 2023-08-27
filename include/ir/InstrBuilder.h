#pragma once

#include "ir/IR.h"

class InstrBuilder {
public:
  InstrBuilder() = default;
  InstrBuilder(Instr &insertionPoint) : insertionPoint(&insertionPoint) {}
  InstrBuilder(Block &insertionBlock)
      : insertionPoint(&insertionBlock.getSentryEnd()) {}

  void emit(Instr *instr) {
    if (insertionPoint) {
      insertionPoint->insertPrev(instr);
    }
  }

  Instr *emitConstInt(IntSSAType &type, int32_t val) {
    Instr *i = new Instr(Instr::CONST_INT);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::IMM32>(val);
    emit(i);
    return i;
  }

  Instr *emitBinop(Instr::Kind kind, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr *i = new Instr(kind);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(lhs.ssaDefType());
    i->emplaceOperand<Operand::SSA_USE>(lhs);
    i->emplaceOperand<Operand::SSA_USE>(rhs);
    emit(i);
    return i;
  }

  PhiInstrPtr emitPhi() {
    Instr *i = new Instr(Instr::INSTR_PHI);
    emit(i);
    return i;
  }

  Instr *emitCmp(BrCond cond, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr *i = new Instr(Instr::INSTR_CMP);
    i->allocateOperands(4);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(IntSSAType::get(1));
    i->emplaceOperand<Operand::BRCOND>(cond);
    i->emplaceOperand<Operand::SSA_USE>(lhs);
    i->emplaceOperand<Operand::SSA_USE>(rhs);
    emit(i);
    return i;
  }

  Instr *emitBr(Block &dst) {
    Instr *i = new Instr(Instr::INSTR_BR);
    i->allocateOperands(1);
    i->emplaceOperand<Operand::SSA_USE>(dst.getDef());
    emit(i);
    return i;
  }

  Instr *emitBrCond(Operand &cond, Block &dstFalse, Block &dstTrue) {
    assert(cond.ssaDefType() == IntSSAType::get(1));
    Instr *i = new Instr(Instr::INSTR_BR_COND);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_USE>(cond);
    i->emplaceOperand<Operand::SSA_USE>(dstFalse.getDef());
    i->emplaceOperand<Operand::SSA_USE>(dstTrue.getDef());
    emit(i);
    return i;
  }

  Instr *emitAdd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_ADD, lhs, rhs);
  }
  Instr *emitSub(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_SUB, lhs, rhs);
  }
  Instr *emitMul(Operand &lhs, Operand &rhs, bool isSigned) {
    return emitBinop(isSigned ? Instr::INSTR_MULS : Instr::INSTR_MULU, lhs,
                     rhs);
  }
  Instr *emitAnd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_AND, lhs, rhs);
  }
  Instr *emitOr(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_OR, lhs, rhs);
  }
  Instr *emitXor(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_XOR, lhs, rhs);
  }

private:
  IntrusiveListNode<Instr, Block> *insertionPoint = nullptr;
};
