#pragma once

#include "ir/IRPass.h"
#include "ir/InstrBuilder.h"

class PhiIsolationPass : public IRPass<Function> {
  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func) {
      for (auto &instr : block) {
        if (!instr.isPhi()) {
          break;
        }
        auto phi = PhiInstrPtr(&instr);
        for (unsigned i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &pred = phi.getPredecessorBlock(i);
          auto builder = InstrBuilder(pred.getLast());
          builder.emitCopy(phi.getPredecessorDef(i));
          phi.setPredecessor(i, builder.getDef(), pred);
        }
        auto builder = InstrBuilder(block.getFirstNonPhiSentry());
        auto &copyInstr = builder.emitCopy(phi->getDef());
        phi->getDef().ssaDef().replaceAllUses(copyInstr.getDef());
        copyInstr.getOperand(1).ssaUseReplace(phi->getDef());
      }
    }
  }

  const char *name() override { return "PhiIsolationPass"; }
};
