#pragma once

#include "ir/IR.h"
#include "ir/IRPass.h"
#include <iostream>
#include <iterator>
#include <ranges>

class IRPatExecutor {
public:
  virtual bool execute(Instr &instr) { return false; }
};

inline bool hasNoUsers(Instr &instr) {
  for (auto &op : std::ranges::subrange(instr.def_begin(), instr.def_end())) {
    if (!op.isSSADef()) {
      continue;
    }
    if (op.ssaDef().hasUses()) {
      return false;
    }
  }
  return true;
}

inline bool isDead(Instr &instr) {
  return !Instr::kindHasSideEffects(instr.getKind()) && hasNoUsers(instr);
}

class InstrSelectorPass : public IRPass<Function> {
public:
  InstrSelectorPass(IRPatExecutor &exec) : exec(&exec) {}

  const char *name() override { return "InstrSelector"; };

  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func | std::views::reverse) {
      for (auto it = --block.end(), itBegin = --block.begin(); it != itBegin;) {
        auto &instr = *it;
        --it;
        if (exec->execute(instr)) {
          instr.deleteThis();
        } else {
          std::cerr << "[ISel] Miss for: " << instr.getKind() << "\n";
        }
      }
    }
  }

  IRPatExecutor *exec;
};

class WorklistPatternExecutionPass : public IRPass<Function> {
  const char *name() override { return "WorklistPatternExecutionPass"; }
  void run(Function &obj, IRInfo<Function> &info) override {}
};
