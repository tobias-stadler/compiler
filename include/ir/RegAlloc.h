#pragma once

#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/RegLiveness.h"
#include <algorithm>
#include <set>

class RegAlloc {
public:
  static const IRInfoID ID;
  std::unordered_map<Reg, Reg> vRegToPhys;
};

class RegAllocPass : public IRPass<Function> {

  const char *name() override { return "RegAllocPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    alloc = RegAlloc();

    RegTracking &regTrack = info.query<RegTracking>();
    LiveIntervals &live = info.query<LiveIntervals>();

    std::unordered_set<Reg> active;

    const ArchRegClass &regClass = *info.getArch().getArchRegClass(1);
    auto archRegs = info.getArch().getArchRegs();

    std::vector<unsigned> regs(archRegs.back().reg + 1);
    std::fill(regs.begin(), regs.end(), true);

    for (Reg currReg = regTrack.begin(); currReg != regTrack.end();
         currReg = currReg + 1) {
      std::cout << "Picking: " << currReg << "\n";
      auto intervalIt = live.intervals.find(currReg);
      if (intervalIt == live.intervals.end())
        continue;
      LiveInterval &currInterval = intervalIt->second;
      if (currInterval.empty())
        continue;
      for (auto it = active.begin(); it != active.end();) {
        LiveInterval &otherInterval = live.intervals[*it];
        if (otherInterval.getLast().to <= currInterval.getFirst().from) {
          auto physRegIt = alloc.vRegToPhys.find(*it);
          if (physRegIt != alloc.vRegToPhys.end()) {
            std::cout << "Freeing: " << physRegIt->second << "\n";
            regs[physRegIt->second] = true;
            it = active.erase(it);
            continue;
          }
        }
        ++it;
      }

      for (auto *archReg : regClass.regs) {
        if (regs[archReg->reg]) {
          std::cout << "Assigning: " << archReg->reg << "\n";
          regs[archReg->reg] = false;
          alloc.vRegToPhys[currReg] = archReg->reg;
          break;
        }
      }

      active.insert(currReg);
    }

    info.publish(alloc);
    info.preserve<RegTracking>();
    info.preserve<LiveIntervals>();
  }

  void invalidate(IRInfo<Function> &info) override { info.retract<RegAlloc>(); }

private:
  RegAlloc alloc;
};

class RegRewritePass : public IRPass<Function> {
  const char *name() override { return "RegRewritePass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    RegTracking &regTrack = info.query<RegTracking>();
    RegAlloc &regAlloc = info.query<RegAlloc>();

    // Rewrite real vRegs
    for (auto &block : func) {
      for (auto &instr : block) {
        for (auto &op : instr) {
          if (!op.isReg())
            continue;

          auto it = regAlloc.vRegToPhys.find(op.reg());
          if (it == regAlloc.vRegToPhys.end()) {
            continue;
          }
          if (op.isRegDef()) {
            op.emplaceRaw<Operand::REG_DEF>(it->second);
          } else {
            op.emplaceRaw<Operand::REG_USE>(it->second);
          }
        }
      }
    }

    // Rewrite SSA Defs
    for (Reg currReg = regTrack.begin(); currReg != regTrack.end();
         currReg = currReg + 1) {
      auto *op = regTrack.getSSADef(currReg);
      auto it = regAlloc.vRegToPhys.find(currReg);
      if (!op)
        continue;
      if (it == regAlloc.vRegToPhys.end())
        continue;
      regTrack.lowerSSADef(currReg);
      op->ssaDefReplace(it->second);
    }
  }
};
