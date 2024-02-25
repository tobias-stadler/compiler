#pragma once

#include "ir/FrameLayout.h"
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
  PhiInstrPtr(Instr &instr) : InstrPtr(&instr) {}
  PhiInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr && instr->getKind() == Instr::PHI);
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

  Block &getBlock() {
    assert(insertionPoint);
    return insertionPoint->getParent();
  }

  Function &getFunction() { return getBlock().getParent(); }

  Operand &getDef(unsigned n = 0) {
    assert(lastInstr);
    return lastInstr->getDef(n);
  }

  Instr &emit(Instr *instr) {
    assert(insertionPoint);
    insertionPoint->insertPrev(instr);
    lastInstr = instr;
    return *instr;
  }

  Instr &emitInstr(unsigned kind) {
    return emit(getFunction().createInstr(kind));
  }

  Instr &emitInstr(unsigned kind, unsigned cap) {
    return emit(getFunction().createInstr(kind, cap));
  }

  Instr &emitConstInt(SSAType &type, int32_t val) {
    // TODO: assert that type can construct constant
    assert(type.getKind() == SSAType::INT || type.getKind() == SSAType::PTR);
    Instr &i = emitInstr(Instr::CONST_INT, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::IMM32>(val);
    return i;
  }

  Instr &emitBinop(Instr::Kind kind, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr &i = emitInstr(kind, 3);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(lhs.ssaDefType());
    i.emplaceOperand<Operand::SSA_USE>(lhs);
    i.emplaceOperand<Operand::SSA_USE>(rhs);
    return i;
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

  PhiInstrPtr emitPhi(SSAType &type) {
    Instr &i = emitInstr(Instr::PHI);
    i.allocateVariadicOperands(1);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    return i;
  }

  Instr &emitExt(Instr::Kind kind, SSAType &type, Operand &val) {
    assert(Instr::kindIsArtifact(kind));
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() >
           static_cast<IntSSAType &>(val.ssaDefType()).getBits());
    Instr &i = emitInstr(kind, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }

  Instr &emitTrunc(SSAType &type, Operand &val) {
    assert(val.ssaDefType().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() <
           static_cast<IntSSAType &>(val.ssaDefType()).getBits());
    Instr &i = emitInstr(Instr::TRUNC, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }

  Instr &emitCopy(Operand &val) {
    assert(val.isSSARegDef());
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(val.ssaDefType());
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }

  Instr &emitCopy(Reg dst, Operand &src) {
    assert(src.isSSARegDef());
    assert(dst);
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::REG_DEF>(dst);
    i.emplaceOperand<Operand::SSA_USE>(src);
    return i;
  }

  Instr &emitCopy(SSAType &type, Reg src) {
    assert(src);
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::REG_USE>(src);
    return i;
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
    Instr &i =
        emitInstr(Instr::CALL, func.returnTypes.size() + 1 + args.size());
    for (auto *ty : func.returnTypes) {
      assert(ty);
      i.emplaceOperand<Operand::SSA_DEF_TYPE>(*ty);
    }
    i.emplaceOperand<Operand::SSA_USE>(func.getDef());
    unsigned argNum = 0;
    for (auto *arg : args) {
      assert(arg && arg->ssaDefType() == *func.paramTypes[argNum]);
      i.emplaceOperand<Operand::SSA_USE>(*arg);
      ++argNum;
    }
    return i;
  }

  Instr &emitReturn(Operand &val) {
    Instr &i = emitInstr(Instr::RET, 1);
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }
  Instr &emitReturn() {
    Instr &i = emitInstr(Instr::RET);
    return i;
  }

  Operand &emitParamRef(SSAType &ty) {
    Instr &i = emitInstr(Instr::REF_PARAM, 1);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(ty);
    return i.getDef();
  }

  Operand &emitOtherSSADefRef(OtherSSADef &def) {
    Instr &i = emitInstr(Instr::REF_OTHERSSADEF, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i.emplaceOperand<Operand::SSA_USE>(def.getDef());
    return i.getDef();
  }

  Instr &emitAlloca(SSAType &eleType, unsigned numEle) {
    Instr &i = emitInstr(Instr::ALLOCA, 3);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i.emplaceOperand<Operand::TYPE>(&eleType);
    i.emplaceOperand<Operand::IMM32>(numEle);
    return i;
  }

  Instr &emitAlloca(SSAType &eleType, Operand &numEleOp) {
    Instr &i = emitInstr(Instr::ALLOCA, 3);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(PtrSSAType::get());
    i.emplaceOperand<Operand::TYPE>(&eleType);
    i.emplaceOperand<Operand::SSA_USE>(numEleOp);
    return i;
  }

  Instr &emitLoad(SSAType &type, Operand &addr) {
    assert(addr.ssaDefType() == PtrSSAType::get());
    Instr &i = emitInstr(Instr::LOAD, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(addr);
    return i;
  }

  Instr &emitStore(Operand &val, Operand &addr) {
    assert(val.getKind() == Operand::SSA_DEF_TYPE);
    assert(addr.ssaDefType() == PtrSSAType::get());
    Instr &i = emitInstr(Instr::STORE, 2);
    i.emplaceOperand<Operand::SSA_USE>(val);
    i.emplaceOperand<Operand::SSA_USE>(addr);
    return i;
  }

  Instr &emitCmp(BrCond cond, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDefType() == rhs.ssaDefType());
    Instr &i = emitInstr(Instr::CMP, 4);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(IntSSAType::get(1));
    i.emplaceOperand<Operand::BRCOND>(cond);
    i.emplaceOperand<Operand::SSA_USE>(lhs);
    i.emplaceOperand<Operand::SSA_USE>(rhs);
    return i;
  }

  Instr &emitBr(Block &dst) {
    Instr &i = emitInstr(Instr::BR, 1);
    i.emplaceOperand<Operand::SSA_USE>(dst.getDef());
    return i;
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
    Instr &i = emitInstr(Instr::BR_COND, 3);
    i.emplaceOperand<Operand::SSA_USE>(cond);
    i.emplaceOperand<Operand::SSA_USE>(dstTrue.getDef());
    i.emplaceOperand<Operand::SSA_USE>(dstFalse.getDef());
    return i;
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
