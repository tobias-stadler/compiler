#pragma once

#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/RegLiveness.h"
#include <set>

class RegAlloc {
public:
  std::unordered_map<Reg, Reg> vRegToPhys;
};

class RegAllocPass : public IRPass<Function> {

  const char *name() override { return "RegAllocPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    RegTracking &regTrack = info.query<RegTracking>();
    LiveIntervals &live = info.query<LiveIntervals>();
    RegAlloc alloc;

    std::set<LiveInterval *> active;

    for (size_t i = 0; i < regTrack.getNumVRegs(); ++i) {
      Reg currReg = regTrack.getFirstVReg() + i;
      auto intervalIt = live.intervals.find(currReg);
      if (intervalIt == live.intervals.end())
        continue;
      LiveInterval &currInterval = intervalIt->second;
      if (currInterval.empty())
        continue;
      for (auto it = active.begin(); it != active.end();) {
        auto tmpIt = it;
        ++it;
        LiveInterval &otherInterval = **tmpIt;
        if (otherInterval.getLast().to <= currInterval.getFirst().from) {
          active.erase(tmpIt);
        }
      }

      for (auto *otherInterval : active) {

      }

      active.insert(&currInterval);
    }

    info.preserve<RegTracking>();
    info.preserve<LiveIntervals>();
  }
};

class RegRewritePass : public IRPass<Function> {
  const char *name() override { return "RegRewritePass"; }

  void run(Function &obj, IRInfo<Function> &info) override {}
};
