#pragma once

#include "ir/IR.h"
#include "support/IntrusiveList.h"
#include <cassert>
#include <numbers>
#include <span>

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
  Operand &getPredecessorUse(unsigned n) {
    return instr->getChainOperand().chain().getOperand(n * 2);
  }
  Operand &getPredecessorDef(unsigned n) {
    return getPredecessorUse(n).ssaUse().getDef();
  }

  bool isComplete() { return instr->getChainOperand().chain().isAllocated(); }

  void setPredecessor(unsigned n, Operand &def, Block &pred) {
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
    assert(type.getKind() == SSAType::INT);
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

  Instr &emitShiftLeft(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SL_L, lhs, rhs);
  }
  Instr &emitShiftRightLogical(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SR_L, lhs, rhs);
  }
  Instr &emitShiftRightArith(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SR_A, lhs, rhs);
  }

  PhiInstrPtr buildPhi(SSAType &type) {
    Instr *i = new Instr(Instr::PHI);
    i->allocateVariadicOperands(1);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    return i;
  }

  Instr &emitExt(Instr::Kind kind, SSAType &type, Operand &val) {
    assert(Instr::kindIsArtifact(kind));
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() >
           static_cast<IntSSAType &>(val.ssaDefType()).getBits());
    Instr *i = new Instr(kind);
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
    assert(val.isSSARegDef());
    Instr *i = new Instr(Instr::COPY);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(val.ssaDefType());
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }

  Instr &emitCopy(Reg dst, Operand &src) {
    assert(src.isSSARegDef());
    assert(dst);
    Instr *i = new Instr(Instr::COPY);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::REG_DEF>(dst);
    i->emplaceOperand<Operand::SSA_USE>(src);
    emit(i);
    return *i;
  }

  Instr &emitCopy(SSAType &type, Reg src) {
    assert(src);
    Instr *i = new Instr(Instr::COPY);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i->emplaceOperand<Operand::REG_USE>(src);
    emit(i);
    return *i;
  }

  Instr &emitExtOrTrunc(Instr::Kind kind, SSAType &type, Operand &val) {
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    unsigned dstBits = static_cast<IntSSAType &>(type).getBits();
    unsigned srcBits = static_cast<IntSSAType &>(val.ssaDefType()).getBits();
    if (dstBits < srcBits) {
      return emitTrunc(type, val);
    } else if (dstBits > srcBits) {
      return emitExt(kind, type, val);
    } else {
      return emitCopy(val);
    }
  }

  Instr &emitCall(Function &func, std::span<Operand *> args) {
    assert(args.size() == func.paramTypes.size() &&
           "Invalid number of arguments");
    Instr *i = new Instr(Instr::CALL);
    i->allocateOperands(func.returnTypes.size() + 1 + args.size());
    for (auto *ty : func.returnTypes) {
      assert(ty);
      i->emplaceOperand<Operand::SSA_DEF_TYPE>(*ty);
    }
    i->emplaceOperand<Operand::SSA_USE>(func.getDef());
    unsigned argNum = 0;
    for (auto *arg : args) {
      assert(arg && arg->ssaDefType() == *func.paramTypes[argNum]);
      ++argNum;
      i->emplaceOperand<Operand::SSA_USE>(*arg);
    }
    emit(i);
    return *i;
  }

  Instr &emitReturn(Operand &val) {
    Instr *i = new Instr(Instr::RET);
    i->allocateOperands(1);
    i->emplaceOperand<Operand::SSA_USE>(val);
    emit(i);
    return *i;
  }
  Instr &emitReturn() {
    Instr *i = new Instr(Instr::RET);
    emit(i);
    return *i;
  }

  Operand &emitParamRef(SSAType &ty) {
    Instr *i = new Instr(Instr::REF_PARAM);
    i->allocateOperands(1);
    i->emplaceOperand<Operand::SSA_DEF_TYPE>(ty);
    emit(i);
    return i->getDef();
  }

  Operand &emitGlobalRef(GlobalDef &global) {
    Instr *i = new Instr(Instr::REF_GLOBAL);
    i->allocateOperands(1);
    i->emplaceOperand<Operand::SSA_DEF_GLOBAL>(global);
    emit(i);
    return i->getDef();
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

  Instr &emitStore(Operand &val, Operand &addr) {
    assert(val.getKind() == Operand::SSA_DEF_TYPE);
    assert(addr.ssaDefType() == PtrSSAType::get());
    Instr *i = new Instr(Instr::STORE);
    i->allocateOperands(2);
    i->emplaceOperand<Operand::SSA_USE>(val);
    i->emplaceOperand<Operand::SSA_USE>(addr);
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
    Instr &i = emitConstInt(val.ssaDefType(), -1);
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
    return emitBinop(isSigned ? Instr::MUL_S : Instr::MUL_U, lhs, rhs);
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
