#pragma once

#include "ir/IR.h"
#include "ir/IRPass.h"
#include <cassert>
#include <iostream>
#include <iterator>
#include <ranges>

class IRObserver {
  virtual void observe(Operand &op) {}
};

class VectorIRObserver : public IRObserver {
  void observe(Operand &op) override { observed.push_back(&op); }

public:
  std::vector<Operand *> observed;
};

class IRPatExecutor {
public:
  virtual bool execute(Instr &instr) { return false; }

  IRObserver *observer = nullptr;
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
  InstrSelectPass(IRPatExecutor &exec) : exec(&exec) {
    exec.observer = &observer;
  }

  const char *name() override { return "InstrSelectPass"; };

  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func | std::views::reverse) {
      for (auto it = --block.end(), itBegin = --block.begin(); it != itBegin;) {
        auto &instr = *it;
        --it;
        execute(instr);
        if (observer.observed.empty()) [[likely]]
          continue;
        for (auto *op : observer.observed) {
          execute(op->getParent());
        }
        observer.observed.clear();
      }
    }
  }

  void execute(Instr &instr) {
    if (instr.isPhi() || instr.isTarget() || instr.isCopy()) {
      return;
    }
    if (exec->execute(instr)) [[likely]] {
      return;
    }
    std::cerr << "[ISel] Miss for: " << Instr::kindName(instr.getKind());
    if (isDead(instr)) {
      std::cerr << ", but dead\n";
      instr.deleteThis();
    } else {
      std::cerr << ", illegal!\n";
      exit(1);
    }
  }

  IRPatExecutor *exec;
  VectorIRObserver observer;
};

class InstrExpansionPass : public IRPass<Function> {
public:
  InstrExpansionPass(IRPatExecutor &exec) : exec(&exec) {}

  const char *name() override { return "InstrExpansionPass"; };

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

  const char *name() override { return "InstrCombinePass"; };

  void run(Function &func, IRInfo<Function> &info) override {
    bool changed = false;
    do {
      changed = false;
      for (auto &block : func | std::views::reverse) {
        for (auto it = --block.end(), itBegin = --block.begin();
             it != itBegin;) {
          auto &instr = *it;
          --it;

          changed |= exec->execute(instr);
        }
      }
    } while (changed);
  }

  IRPatExecutor *exec;
};
