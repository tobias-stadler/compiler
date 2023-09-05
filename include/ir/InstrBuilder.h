#pragma once

#include "ir/IR.h"
#include <cassert>

class InstrPtr {
public:
  InstrPtr(Instr *instr) : instr(instr) { assert(instr); }

  Instr &operator*() { return *instr; }
  Instr *operator->() { return instr; }
  Instr *get() { return instr; }

protected:
  Instr *instr;
};

class PhiInstrPtr : public InstrPtr {
public:
  PhiInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr->getKind() == Instr::INSTR_PHI);
  }

  void setupOperands(unsigned numPred) {
    instr->allocateOperands(1 + 2 * numPred);
    instr->emplaceOperand<Operand::SSA_DEF_TYPE>(VoidSSAType::get());
  }

  void setType(SSAType &type) { instr->getDef().ssaDefSetType(type); }

  void addPred(Operand &def, Block &pred) {
    assert(def.ssaDefType() == instr->getDef().ssaDefType());
    instr->emplaceOperand<Operand::SSA_USE>(def);
    instr->emplaceOperand<Operand::BLOCK>(&pred);
  }
};

class InstrBuilder {
public:
  InstrBuilder() = default;
  InstrBuilder(Instr &insertionPoint) : insertionPoint(&insertionPoint) {}
  InstrBuilder(Block &insertionPoint)
      : insertionPoint(&insertionPoint.getSentryEnd()) {}

  void setInsertionPoint(Instr *instr) { insertionPoint = instr; }
  void setInsertionPoint(Instr &instr) { insertionPoint = &instr; }
  void setInsertionPoint(Block &block) {
    insertionPoint = &block.getSentryEnd();
  }

  Instr* getLastInstr() {
    return lastInstr;
  }

  void emit(Instr *instr) {
    assert(insertionPoint);
    insertionPoint->insertPrev(instr);
    lastInstr = instr;
  }

  Instr &emitConstInt(IntSSAType &type, int32_t val) {
    Instr *i = new Instr(Instr::CONST_INT);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::IMM32>(val);
    emit(i);
    return *i;
  }

  Instr &emitBinop(Instr::Kind kind, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr *i = new Instr(kind);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(lhs.ssaDefType());
    i->emplaceOperand<Operand::SSA_USE>(lhs);
    i->emplaceOperand<Operand::SSA_USE>(rhs);
    emit(i);
    return *i;
  }

  PhiInstrPtr buildPhi() {
    Instr *i = new Instr(Instr::INSTR_PHI);
    return i;
  }

  Instr &emitCmp(BrCond cond, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr *i = new Instr(Instr::INSTR_CMP);
    i->allocateOperands(4);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(IntSSAType::get(1));
    i->emplaceOperand<Operand::BRCOND>(cond);
    i->emplaceOperand<Operand::SSA_USE>(lhs);
    i->emplaceOperand<Operand::SSA_USE>(rhs);
    emit(i);
    return *i;
  }

  Instr &emitBr(Block &dst) {
    Instr *i = new Instr(Instr::INSTR_BR);
    i->allocateOperands(1);
    i->emplaceOperand<Operand::SSA_USE>(dst.getDef());
    emit(i);
    return *i;
  }

  Instr &emitBrCond(Operand &cond, Block &dstFalse, Block &dstTrue) {
    assert(cond.ssaDefType() == IntSSAType::get(1));
    Instr *i = new Instr(Instr::INSTR_BR_COND);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_USE>(cond);
    i->emplaceOperand<Operand::SSA_USE>(dstFalse.getDef());
    i->emplaceOperand<Operand::SSA_USE>(dstTrue.getDef());
    emit(i);
    return *i;
  }

  Instr &emitAdd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_ADD, lhs, rhs);
  }
  Instr &emitSub(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_SUB, lhs, rhs);
  }
  Instr &emitMul(Operand &lhs, Operand &rhs, bool isSigned) {
    return emitBinop(isSigned ? Instr::INSTR_MULS : Instr::INSTR_MULU, lhs,
                     rhs);
  }
  Instr &emitAnd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_AND, lhs, rhs);
  }
  Instr &emitOr(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_OR, lhs, rhs);
  }
  Instr &emitXor(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_XOR, lhs, rhs);
  }

private:
  IntrusiveListNode<Instr, Block> *insertionPoint = nullptr;
  Instr *lastInstr = nullptr;
};