#pragma once

#include "ir/ArchInstrBuilder.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/RegLiveness.h"
#include "support/Ranges.h"
#include <algorithm>
#include <cassert>
#include <set>

class RegAlloc {
public:
  static const IRInfoID ID;

  struct Assignment {
    enum Kind {
      NONE,
      PHYS_REG,
      NO_REG,
    };

    Kind kind = NONE;
    Reg reg;
    size_t spillCost;
  };

  RegAlloc() {}
  RegAlloc(size_t numVRegs) : vRegs(numVRegs) {}

  VRegMap<Assignment> vRegs;
};

/*
class LinearRegAllocPass : public IRPass<Function> {

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
*/

class RegRewritePass : public IRPass<Function> {
  const char *name() override { return "RegRewritePass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    RegAlloc &regAlloc = info.query<RegAlloc>();

    for (auto &block : func) {
      for (auto &instr : block) {
        for (auto &op : instr) {
          if (!op.isReg() || !op.reg().isVReg())
            continue;
          auto &regAssign = regAlloc.vRegs[op.reg()];
          assert(regAssign.kind == RegAlloc::Assignment::PHYS_REG);
          op.regDefUse().replace(regAssign.reg);
        }
      }
    }
  }
};

class ColoringRegAllocPass : public IRPass<Function> {

  const char *name() override { return "ColoringRegAllocPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    this->func = &func;
    printer = &info.query<PrintIRVisitor>();
    arch = &info.getArch();
    regInfo = &func.getRegInfo();

    const ArchRegClass &regClass = *arch->getArchRegClass(1);

    bool failed;
    do {
      graph = &info.query<LiveGraph>();
      alloc = RegAlloc(graph->getNumVRegs());
      virtSet = VLiveSet(graph->getNumVRegs());
      physSet = PhysLiveSet(graph->getNumPhysRegs());
      graph = &info.query<LiveGraph>();
      simplify(regClass.regs.size());
      selectAll();
      spillAll();
      failed = false;
      for (auto [reg, regAssign] : alloc.vRegs) {
        if (regAssign.kind != RegAlloc::Assignment::PHYS_REG) {
          failed = true;
          break;
        }
      }
      info.invalidate<LiveFlow>();
      info.invalidate<LiveGraph>();
    } while (failed);

    info.publish(alloc);
  }

  void simplify(size_t k) {
    std::cout << "simplify\n";
    eliminationOrder.clear();
    graph->resetIgnore();
    virtSet.clear();
    for (auto [reg, vLive] : graph->vRegs) {
      if (vLive.degree < k) {
        eliminate(reg);
      } else {
        virtSet.insert(reg);
      }
    }
    calcSpillCost();
    while (!virtSet.empty()) {
      Reg spillReg = Reg();
      for (auto reg : virtSet) {
        if (graph->vRegs[reg].degree < k) {
          eliminate(reg);
          virtSet.erase(reg);
          spillReg = Reg();
          break;
        }
        if (!spillReg ||
            alloc.vRegs[reg].spillCost < alloc.vRegs[spillReg].spillCost) {
          spillReg = reg;
        }
      }
      if (!spillReg)
        continue;
      eliminate(spillReg);
      virtSet.erase(spillReg);
      std::cout << "  might spill " << spillReg.getIdx() << " with cost "
                << alloc.vRegs[spillReg].spillCost << "\n";
    }
  }

  void eliminate(Reg reg) {
    eliminationOrder.push_back(reg);
    graph->ignore(reg);
  }

  void calcSpillCost() {
    for (auto reg : virtSet) {
      alloc.vRegs[reg].spillCost = regInfo->defUseRoot(reg).count();
    }
  }

  void selectAll() {
    for (auto reg : eliminationOrder | std::views::reverse) {
      select(reg);
    }
  }

  void spillAll() {
    for (auto [reg, regAssign] : alloc.vRegs) {
      if (regAssign.kind != RegAlloc::Assignment::NO_REG)
        continue;
      spillGlobal(reg);
    }
  }

  void select(Reg reg) {
    std::cout << "select " << reg.getIdx() << ": ";
    const ArchRegClass &regClass = *arch->getArchRegClass(1);
    physSet.clear();

    auto &regLive = graph->vRegs[reg];
    auto &regAssign = alloc.vRegs[reg];
    physSet.insert(regLive.phys);
    for (auto oReg : regLive.virt) {
      auto &oRegAssign = alloc.vRegs[oReg];
      if (oRegAssign.kind != RegAlloc::Assignment::PHYS_REG)
        continue;
      physSet.insert(oRegAssign.reg);
    }
    for (auto *archReg : regClass.regs) {
      if (!physSet.contains(archReg->reg)) {
        std::cout << "assign " << archReg->name << "\n";
        regAssign.kind = RegAlloc::Assignment::PHYS_REG;
        regAssign.reg = archReg->reg;
        return;
      }
    }
    std::cout << "no phys reg available\n";
    regAssign.kind = RegAlloc::Assignment::NO_REG;
  }

  void spillGlobal(Reg reg) {
    std::cout << "global spill " << reg.getIdx() << "\n";
    auto &archIB = arch->getArchInstrBuilder();
    // FIXME: proper size calculation
    auto &frameDef = func->getFrameLayout().createFrameEntry(4, Alignment(2));
    for (auto &op : make_earlyincr_range(regInfo->defUseRoot(reg))) {
      Reg newReg = regInfo->cloneVReg(reg);
      op.regDefUse().replace(newReg);
      if (op.isRegDef()) {
        InstrBuilder IB(op.getParent().getNextNode());
        archIB.frameStoreReg(IB, newReg, frameDef);
      } else {
        InstrBuilder IB(op.getParent());
        archIB.frameLoadReg(IB, newReg, frameDef);
      }
    }
  }

  void invalidate(IRInfo<Function> &info) override { info.retract<RegAlloc>(); }

private:
  RegAlloc alloc;
  PhysLiveSet physSet;
  VLiveSet virtSet;
  std::vector<Reg> eliminationOrder;
  Function *func;
  RegInfo *regInfo;
  Arch *arch;
  LiveGraph *graph;
  PrintIRVisitor *printer;
};
