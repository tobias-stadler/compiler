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
    assert(instr->getKind() == Instr::PHI);
  }

  void setupPredecessors(unsigned numPreds) {
    instr->getChainOperand().chain().allocate(2 * numPreds);
  }

  unsigned getNumPredecessors() {
    return instr->getChainOperand().chain().getCapacity() / 2;
  }

  Block &getPredecessorBlock(unsigned n) {
    return instr->getChainOperand().chain().getOperand(n * 2 + 1).block();
  }
  Operand &getPredecessorDef(unsigned n) {
    return instr->getChainOperand().chain().getOperand(n * 2).ssaUse().getDef();
  }

  bool isComplete() { return instr->getChainOperand().chain().isAllocated(); }

  void setPredecessor(unsigned n, Operand &def, Block &pred) {
    assert(def.ssaDefType() == instr->getDef().ssaDefType());
    OperandChain &chain = instr->getChainOperand().chain();
    chain.getOperand(n * 2).emplace<Operand::SSA_USE>(instr, def);
    chain.getOperand(n * 2 + 1).emplace<Operand::BLOCK>(instr, &pred);
  }
};

class AllocaInstrPtr : public InstrPtr {
  AllocaInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr->getKind() == Instr::ALLOCA);
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

  Instr &emitConstInt(SSAType &type, int32_t val) {
    // TODO: assert that type can construct constant
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

  PhiInstrPtr buildPhi(SSAType &type) {
    Instr *i = new Instr(Instr::PHI);
    i->allocateVariadicOperands(1);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    return i;
  }

  Instr &emitExt(SSAType &type, Operand &val, bool isSigned = false) {
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() >
           static_cast<IntSSAType &>(val.ssaDefType()).getBits());
    Instr *i = new Instr(isSigned ? Instr::EXT_S : Instr::EXT_Z);
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
    Instr *i = new Instr(Instr::TRUNC);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }

  Instr &emitCopy(Operand &val) {
    Instr *i = new Instr(Instr::COPY);
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
    Instr *i = new Instr(Instr::ALLOCA);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i->emplaceOperand<Operand::TYPE>(&eleType);
    i->emplaceOperand<Operand::IMM32>(numEle);
    emit(i);
    return *i;
  }

  Instr &emitAlloca(SSAType &eleType, Operand &numEleOp) {
    Instr *i = new Instr(Instr::ALLOCA);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i->emplaceOperand<Operand::TYPE>(&eleType);
    i->emplaceOperand<Operand::SSA_USE>(numEleOp);
    emit(i);
    return *i;
  }

  Instr &emitLoad(SSAType &type, Operand &addr) {
    assert(addr.ssaDefType() == PtrSSAType::get());
    Instr *i = new Instr(Instr::LOAD);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::SSA_USE>(addr);
    emit(i);
    return *i;
  }

  Instr &emitStore(Operand &addr, Operand &val) {
    assert(addr.ssaDefType() == PtrSSAType::get());
    assert(val.getKind() == Operand::SSA_DEF_TYPE);
    Instr *i = new Instr(Instr::STORE);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_USE>(addr);
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }

  Instr &emitCmp(BrCond cond, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr *i = new Instr(Instr::CMP);
    i->allocateOperands(4);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(IntSSAType::get(1));
    i->emplaceOperand<Operand::BRCOND>(cond);
    i->emplaceOperand<Operand::SSA_USE>(lhs);
    i->emplaceOperand<Operand::SSA_USE>(rhs);
    emit(i);
    return *i;
  }

  Instr &emitBr(Block &dst) {
    Instr *i = new Instr(Instr::BR);
    i->allocateOperands(1);
    i->emplaceOperand<Operand::SSA_USE>(dst.getDef());
    emit(i);
    return *i;
  }

  Instr &emitNot(Operand &val) {
    // TODO: arbitrary int
    Instr &i = emitConstInt(IntSSAType::get(1), 1);
    return emitXor(val, i.getDef());
  }

  Instr &emitNeg(Operand &val) {
    Instr &i = emitConstInt(val.ssaDefType(), 0);
    return emitSub(i.getDef(), val);
  }

  Instr &emitBrCond(Operand &cond, Block &dstTrue, Block &dstFalse) {
    assert(cond.ssaDefType() == IntSSAType::get(1));
    Instr *i = new Instr(Instr::BR_COND);
    i->allocateOperands(3);
    i->emplaceOperand<Operand::SSA_USE>(cond);
    i->emplaceOperand<Operand::SSA_USE>(dstTrue.getDef());
    i->emplaceOperand<Operand::SSA_USE>(dstFalse.getDef());
    emit(i);
    return *i;
  }

  Instr &emitAdd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::ADD, lhs, rhs);
  }
  Instr &emitSub(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SUB, lhs, rhs);
  }
  Instr &emitMul(Operand &lhs, Operand &rhs, bool isSigned) {
    return emitBinop(isSigned ? Instr::MUL_S : Instr::MUL_U, lhs,
                     rhs);
  }
  Instr &emitAnd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::AND, lhs, rhs);
  }
  Instr &emitOr(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::OR, lhs, rhs);
  }
  Instr &emitXor(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::XOR, lhs, rhs);
  }

private:
  IntrusiveListNode<Instr, Block> *insertionPoint = nullptr;
  Instr *lastInstr = nullptr;
};
