#pragma once

#include "ir/IR.h"
#include "support/IntrusiveList.h"
#include <cassert>
#include <numbers>

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

class AllocaInstrPtr : public InstrPtr {
  AllocaInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr->getKind() == Instr::INSTR_ALLOCA);
  }
};

class InstrBuilder {
public:
  InstrBuilder() = default;
  InstrBuilder(IntrusiveListNode<Instr, Block> &insertionPoint)
      : insertionPoint(&insertionPoint) {}
  InstrBuilder(IntrusiveListNode<Instr, Block> *insertionPoint)
      : insertionPoint(insertionPoint) {}
  InstrBuilder(Block &insertionPoint)
      : insertionPoint(&insertionPoint.getSentryEnd()) {}

  void setInsertionPoint(IntrusiveListNode<Instr, Block> *node) {
    insertionPoint = node;
  }
  void setInsertionPoint(IntrusiveListNode<Instr, Block> &node) {
    insertionPoint = &node;
  }
  void setInsertionPoint(Block &block) {
    insertionPoint = &block.getSentryEnd();
  }

  Instr *getLastInstr() { return lastInstr; }

  Operand &getDef(unsigned n = 0) {
    assert(lastInstr);
    return lastInstr->getDef(n);
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

  Instr &emitExt(SSAType &type, Operand &val, bool isSigned = false) {
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() >
           static_cast<IntSSAType &>(val.ssaDefType()).getBits());
    Instr *i = new Instr(isSigned ? Instr::INSTR_EXTS : Instr::INSTR_EXTZ);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }

  Instr &emitTrunc(SSAType &type, Operand &val) {
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() <
           static_cast<IntSSAType &>(val.ssaDefType()).getBits());
    Instr *i = new Instr(Instr::INSTR_TRUNC);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }

  Instr &emitCopy(Operand &val) {
    Instr *i = new Instr(Instr::INSTR_COPY);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(val.ssaDefType());
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }

  Instr &emitExtOrTrunc(SSAType &type, Operand &val, bool isSigned = false) {
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    unsigned dstBits = static_cast<IntSSAType &>(type).getBits();
    unsigned srcBits = static_cast<IntSSAType &>(val.ssaDefType()).getBits();
    if (dstBits < srcBits) {
      return emitTrunc(type, val);
    } else if (dstBits > srcBits) {
      return emitExt(type, val, isSigned);
    } else {
      return emitCopy(val);
    }
  }

  Instr &emitAlloca(SSAType &eleType, unsigned numEle) {
    Instr *i = new Instr(Instr::INSTR_ALLOCA);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i->emplaceOperand<Operand::TYPE>(&eleType);
    i->emplaceOperand<Operand::IMM32>(numEle);
    emit(i);
    return *i;
  }

  Instr &emitAlloca(SSAType &eleType, Operand &numEleOp) {
    Instr *i = new Instr(Instr::INSTR_ALLOCA);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i->emplaceOperand<Operand::TYPE>(&eleType);
    i->emplaceOperand<Operand::SSA_USE>(numEleOp);
    emit(i);
    return *i;
  }

  Instr &emitLoad(SSAType &type, Operand &addr) {
    assert(addr.ssaDefType() == PtrSSAType::get());
    Instr *i = new Instr(Instr::INSTR_LOAD);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::SSA_USE>(addr);
    emit(i);
    return *i;
  }

  Instr &emitStore(Operand &addr, Operand &val) {
    assert(addr.ssaDefType() == PtrSSAType::get());
    assert(val.getKind() == Operand::SSA_DEF_TYPE);
    Instr *i = new Instr(Instr::INSTR_STORE);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_USE>(addr);
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
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

  Instr &emitBrCond(Operand &cond, Block &dstTrue, Block &dstFalse) {
    assert(cond.ssaDefType() == IntSSAType::get(1));
    Instr *i = new Instr(Instr::INSTR_BR_COND);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_USE>(cond);
    i->emplaceOperand<Operand::SSA_USE>(dstTrue.getDef());
    i->emplaceOperand<Operand::SSA_USE>(dstFalse.getDef());
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
