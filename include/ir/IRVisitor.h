#pragma once
#include "ir/IR.h"

template <class D> class IRVisitor {
public:
  void dispatch(Function *func) {
    if (func) {
      dispatch(*func);
    }
  }
  void dispatch(Block *block) {
    if (block) {
      dispatch(*block);
    }
  }
  void dispatch(Instr *instr) {
    if (instr) {
      dispatch(*instr);
    }
  }
  void dispatch(Function &func) { impl().visitFunction(func); }
  void dispatch(Block &block) { impl().visitBlock(block); }
  void dispatch(Instr &instr) {
    switch (instr.getKind()) {
    default:
      dispatchDefault(instr);
      break;
    }
  }
  void dispatch(Operand &op) {
    switch (op.getKind()) {
    case Operand::EMPTY:
      break;
    case Operand::SSA_DEF_TYPE:
    case Operand::SSA_DEF_REGCLASS:
    case Operand::SSA_DEF_BLOCK:
    case Operand::SSA_DEF_FUNCTION:
      impl().visitOperandSSADef(op);
      break;
    case Operand::SSA_USE:
      impl().visitOperandSSAUse(op);
      break;
    case Operand::IMM32:
      impl().visitOperandImm32(op);
      break;
    default:
      dispatchDefault(op);
      break;
    }
  }

  void dispatchDefault(Instr &instr) { impl().visitInstr(instr); }
  void dispatchDefault(Operand &op) { impl().visitOperand(op); }

protected:
  D &impl() { return static_cast<D &>(*this); }
  void visitFunction(Function &func) {
    for (auto &b : func) {
      dispatch(b);
    }
  }

  void visitBlock(Block &block) {
    for (auto &i : block) {
      dispatch(i);
    }
  }

  void visitInstr(Instr &instr) {
    for (auto &op : instr) {
      dispatch(op);
    }
  }

  void visitOperand(Operand &) {}
  void visitOperandSSADef(Operand &op) { dispatchDefault(op); }
  void visitOperandSSAUse(Operand &op) { dispatchDefault(op); }
  void visitOperandImm32(Operand &op) { dispatchDefault(op); }
};
