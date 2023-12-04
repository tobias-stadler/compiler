#pragma once

#include "ir/IRPass.h"
#include "ir/InstrBuilder.h"
#include "ir/RegTracking.h"

class PhiIsolationPass : public IRPass<Function> {
  const char *name() override { return "PhiIsolationPass"; }

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
};

class PhiDestructionPass : public IRPass<Function> {
  const char *name() override { return "PhiDestructionPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    auto &regTrack = info.query<RegTracking>();

    for (auto &block : func) {
      for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
        auto &instr = *it;
        ++it;
        if (!instr.isPhi()) {
          break;
        }
        Reg reg = regTrack.getRegForSSADef(instr.getDef());
        regTrack.lowerSSADef(reg);
        auto phi = PhiInstrPtr(&instr);
        for (unsigned i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &def = phi.getPredecessorDef(i);
          regTrack.lowerSSADef(regTrack.getRegForSSADef(def));
          def.ssaDefReplace(reg);
        }
        instr.getDef().ssaDefReplace(reg);
        instr.deleteThis();
      }
    }

    info.preserve<RegTracking>();
  }
};
