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

class InstrSelectPass : public IRPass<Function> {
public:
  InstrSelectPass(IRPatExecutor &exec) : exec(&exec) {}

  const char *name() override { return "InstrSelector"; };

  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func | std::views::reverse) {
      for (auto it = --block.end(), itBegin = --block.begin(); it != itBegin;) {
        auto &instr = *it;
        --it;

        if (instr.isPhi() || instr.isTarget()) {
          continue;
        }
        if (!exec->execute(instr)) {
          std::cerr << "[ISel] Miss for: " << Instr::kindName(instr.getKind());
          if (isDead(instr)) {
            std::cerr << ", but dead";
            instr.deleteThis();
          }
          std::cerr << "\n";
        }
      }
    }
  }

  IRPatExecutor *exec;
};

class InstrExpansionPass : public IRPass<Function> {
public:
  InstrExpansionPass(IRPatExecutor &exec) : exec(&exec) {}

  const char *name() override { return "InstrExpansion"; };

  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func | std::views::reverse) {
      for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
        auto &instr = *it;
        ++it;

        exec->execute(instr);
      }
    }
  }

  IRPatExecutor *exec;
};

class InstrCombinePass : public IRPass<Function> {
public:
  InstrCombinePass(IRPatExecutor &exec) : exec(&exec) {}

  const char *name() override { return "InstrCombiner"; };

  void run(Function &func, IRInfo<Function> &info) override {
    bool Changed = false;
    do {
      for (auto &block : func | std::views::reverse) {
        for (auto it = --block.end(), itBegin = --block.begin();
             it != itBegin;) {
          auto &instr = *it;
          --it;

          Changed |= exec->execute(instr);
        }
      }
    } while (Changed);
  }

  IRPatExecutor *exec;
};
