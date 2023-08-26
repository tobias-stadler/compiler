#pragma once
#include <ir/IR.h>

template <class T> class IRVisitor {
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

  T &impl() { return static_cast<T &>(*this); }

  void dispatch(Function &func) { impl().visitFunction(func); }
  void dispatch(Block &block) { impl().visitBlock(block); }

  void dispatch(Instr &instr) {
    switch (instr.getKind()) {
    default:
      impl().visitInstr(instr);
    }
  }

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

  void visitInstr(Instr &instr) {}
};
