#pragma once

#include "ir/IR.h"
#include "ir/IRPass.h"
#include <iostream>
#include <ranges>

class IRPatExecutor {
public:
  virtual bool execute(Instr &instr) { return false; }
};

class InstrSelectorPass : public IRPass<Function> {
public:
  InstrSelectorPass(IRPatExecutor &exec) : exec(&exec) {}

  const char *name() override { return "InstrSelector"; };

  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func | std::views::reverse) {
      for (auto &instr : block | std::views::reverse) {
        if (!exec->execute(instr)) {
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
