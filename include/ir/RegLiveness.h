#pragma once

#include "ir/IRPass.h"
#include "ir/IRPrinter.h"
#include "ir/InstrBuilder.h"
#include "ir/RegTracking.h"
#include <cassert>
#include <compare>
#include <iostream>
#include <ranges>
#include <unordered_set>

class BlockLivenessFlow {
public:
  std::unordered_set<Reg> liveIn;
  std::unordered_set<Reg> liveOut;
  std::vector<Reg> liveOutNew;
  std::unordered_set<Reg> defs;
};

class LivenessFlow {
public:
  static const IRInfoID ID;
  std::unordered_map<Block *, BlockLivenessFlow> blockLiveness;
};

class LivenessFlowPass : public IRPass<Function> {
public:
  const char *name() override { return "LivenessFlowPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    data.blockLiveness.clear();
    arch = &info.getArch();
    regTrack = &info.query<RegTracking>();

    std::vector<Block *> workList;
    for (auto &block : func) {
      precomputeBlock(block);
      workList.push_back(&block);
    }
    while (!workList.empty()) {
      auto &block = *workList.back();
      workList.pop_back();
      dataflowStep(block, workList);
    }
    info.publish(data);
    info.preserveAll();
  }

  void precomputeBlock(Block &block) {
    std::unordered_set<Reg> regExposed;
    auto &blockLive = data.blockLiveness[&block];
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi()) {
        auto phi = PhiInstrPtr(&instr);
        for (int i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &predBlockLive = data.blockLiveness[&phi.getPredecessorBlock(i)];
          predBlockLive.liveOutNew.push_back(
              regTrack->getRegForSSADef(phi.getPredecessorDef(i)));
        }
        Reg reg = regTrack->getRegForSSADef(instr.getDef());
        blockLive.liveIn.insert(reg);
        regExposed.erase(reg);
        continue;
      }
      for (auto &op : instr) {
        if (op.isSSARegUse()) {
          regExposed.insert(regTrack->getRegForSSADef(op.ssaUse().getDef()));
        } else if (op.isSSARegDef()) {
          Reg reg = regTrack->getRegForSSADef(op);
          blockLive.defs.insert(reg);
          regExposed.erase(reg);
        } else if (op.isRegUse()) {
          if (ignoreReg(*arch, op.reg()))
            continue;
          regExposed.insert(op.reg());
        } else if (op.isRegDef()) {
          if (ignoreReg(*arch, op.reg()))
            continue;
          blockLive.defs.insert(op.reg());
          regExposed.erase(op.reg());
        }
      }
    }
    blockLive.liveIn.insert(regExposed.begin(), regExposed.end());
    for (auto &blockUse : block.getDef().ssaDef()) {
      Block &pred = blockUse.getParentBlock();
      auto &predBlockLive = data.blockLiveness[&pred];
      predBlockLive.liveOutNew.insert(predBlockLive.liveOutNew.end(),
                                      regExposed.begin(), regExposed.end());
    }
  }

  static bool ignoreReg(Arch &arch, Reg reg) {
    const ArchReg *archReg = arch.getArchReg(reg);
    return archReg ? archReg->noLiveness : false;
  }

  void dataflowStep(Block &block, std::vector<Block *> &workList) {
    auto &blockLive = data.blockLiveness[&block];
    for (auto op : blockLive.liveOutNew) {
      if (!blockLive.liveOut.insert(op).second) {
        continue;
      }
      if (blockLive.defs.find(op) != blockLive.defs.end()) {
        continue;
      }
      if (!blockLive.liveIn.insert(op).second) {
        continue;
      }
      for (auto &blockUse : block.getDef().ssaDef()) {
        Block &pred = blockUse.getParentBlock();
        auto &predBlockLive = data.blockLiveness[&pred];
        predBlockLive.liveOutNew.push_back(op);
        workList.push_back(&pred);
      }
    }
    blockLive.liveOutNew.clear();
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LivenessFlow>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LivenessFlow>();
    data.blockLiveness.clear();
  }

private:
  LivenessFlow data;
  Arch *arch;
  RegTracking *regTrack;
};

class PrintLivenessFlowPass : public IRPass<Function> {
  const char *name() { return "PrintLivenessFlowPass"; }

  void run(Function &func, IRInfo<Function> &info) {
    auto regLive = info.query<LivenessFlow>();
    auto printer = info.query<PrintIRVisitor>();
    std::cout << "-- LivenessFlow --\n";
    for (auto &block : func) {
      auto blockLive = regLive.blockLiveness[&block];
      printer.printNumberedDef(block.getDef());
      std::cout << ":\n";

      std::cout << "LiveIn:";
      print(blockLive.liveIn, printer);
      std::cout << "LiveOut:";
      print(blockLive.liveOut, printer);
    }
    std::cout << "----\n";
    info.preserveAll();
  }

  void print(std::unordered_set<Reg> regs, PrintIRVisitor &printer) {
    for (auto x : regs) {
      std::cout << " ";
      printer.printReg(x);
    }
    std::cout << "\n";
  }
};

class LivePos {
public:
  enum Slot {
    EARLY,
    BASE,
    LATE,
  };
  static constexpr const char *slotName(Slot s) {
    switch (s) {
    case EARLY:
      return "e";
    case BASE:
      return "b";
    case LATE:
      return "l";
    }
    return nullptr;
  }

  constexpr LivePos(size_t num, Slot slot = BASE) : num(num), slot(slot) {}
  size_t num;
  Slot slot;

  friend std::strong_ordering operator<=>(LivePos a, LivePos b) {
    auto numCmp = a.num <=> b.num;
    if (numCmp == numCmp.equal) {
      return a.slot <=> b.slot;
    }
    return a.num <=> b.num;
  }
  friend bool operator==(LivePos a, LivePos b) { return a.num == b.num; }
};

class LiveInterval;
class LiveRange : public IntrusiveListNode<LiveRange, LiveInterval> {
public:
  LiveRange(LivePos from, LivePos to) : from(from), to(to) {}
  LivePos from;
  LivePos to;
};

class LiveInterval : public IntrusiveList<LiveRange, LiveInterval> {
public:
  void addRangeCoalesceBegin(LivePos from, LivePos to) {
    assert(from < to);
    if (!empty()) {
      if (to == getFirst().from) {
        getFirst().from = from;
        return;
      }
      assert(to < getFirst().from);
    }
    insertBegin(new LiveRange(from, to));
  }

  void updateFromBegin(LivePos from) {
    assert(!empty());
    assert(from > getFirst().from);
    getFirst().from = from;
  }
};

class LiveIntervals {
public:
  static const IRInfoID ID;
  std::unordered_map<void *, size_t> numbering;
  size_t currNum = 0;
  std::unordered_map<Reg, LiveInterval> intervals;

  void number(void *ptr) {
    currNum += 10;
    numbering[ptr] = currNum;
  }

  size_t getNum(void *ptr) {
    auto it = numbering.find(ptr);
    assert(it != numbering.end());
    return it->second;
  }

  size_t getNumInstr(Instr &instr) { return getNum(&instr); }
  size_t getNumLiveIn(Block &block) { return getNum(&block.getSentryBegin()); }
  size_t getNumLiveOut(Block &block) {
    return getNum(block.hasNext() ? &block.getNext().getSentryBegin()
                                  : &block.getSentryEnd());
  }
};

class LiveIntervalsPass : public IRPass<Function> {
  const char *name() override { return "LiveIntervalsPass"; }
  void run(Function &func, IRInfo<Function> &info) override {
    arch = &info.getArch();
    data = LiveIntervals();
    regTrack = &info.query<RegTracking>();
    flow = &info.query<LivenessFlow>();
    numberInstructions(func);
    buildIntervals(func);
    info.publish(data);
    info.preserveAll();
  }

  void numberInstructions(Function &func) {
    for (auto &block : func) {
      data.number(&block.getSentryBegin());
      for (auto &instr : block) {
        data.number(&instr);
      }
      data.number(&block.getSentryEnd());
    }
  }

  void buildIntervals(Function &func) {
    for (auto &block : func | std::views::reverse) {
      auto &blockLive = flow->blockLiveness[&block];
      auto live = blockLive.liveOut;
      auto blockIn = data.getNumLiveIn(block);
      auto blockOut = data.getNumLiveOut(block);
      for (auto reg : live) {
        data.intervals[reg].addRangeCoalesceBegin(blockIn, blockOut);
      }
      for (auto &instr : block | std::views::reverse) {
        if (instr.isPhi())
          continue;
        for (auto &op : instr) {
          if (op.isSSARegUse()) {
            Reg reg = regTrack->getRegForSSADef(op.ssaUse().getDef());
            useReg(reg, instr, live);
          } else if (op.isSSARegDef()) {
            Reg reg = regTrack->getRegForSSADef(op);
            defReg(reg, instr, live);
          } else if (op.isRegUse()) {
            if (LivenessFlowPass::ignoreReg(*arch, op.reg()))
              continue;
            useReg(op.reg(), instr, live);
          } else if (op.isRegDef()) {
            if (LivenessFlowPass::ignoreReg(*arch, op.reg()))
              continue;
            defReg(op.reg(), instr, live);
          }
        }
      }
    }
  }

  void useReg(Reg reg, Instr &instr, std::unordered_set<Reg> &live) {
    auto blockIn = data.getNumLiveIn(instr.getParent());
    auto instrNum = data.getNumInstr(instr);
    if (live.insert(reg).second) {
      data.intervals[reg].addRangeCoalesceBegin(blockIn, instrNum);
    }
  }

  void defReg(Reg reg, Instr &instr, std::unordered_set<Reg> &live) {
    auto instrNum = data.getNumInstr(instr);
    if (live.erase(reg)) {
      data.intervals[reg].updateFromBegin(instrNum);
    } else {
      data.intervals[reg].addRangeCoalesceBegin(instrNum,
                                                {instrNum, LivePos::LATE});
    }
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveIntervals>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveIntervals>();
  }

  LivenessFlow *flow;
  RegTracking *regTrack;
  Arch *arch;
  LiveIntervals data;
};

class PrintLiveIntervalsPass : public IRPass<Function> {
  const char *name() override { return "PrintLiveIntervalsPass"; }

  void run(Function &obj, IRInfo<Function> &info) override {
    LiveIntervals &live = info.query<LiveIntervals>();
    PrintIRVisitor printer(info.getArchUnchecked());
    printer.setPreInstrCallback(
        [&](Instr &i) { std::cout << live.getNumInstr(i) << ": "; });
    printer.dispatch(obj);
    std::cout << "-- LiveIntervals --\n";
    for (auto &[reg, interval] : live.intervals) {
      printer.printReg(reg);
      std::cout << ":";
      printInterval(interval);
      std::cout << "\n";
    }
    std::cout << "----\n";
    info.preserveAll();
  }

  void printInterval(LiveInterval &interval) {
    for (auto &r : interval) {
      std::cout << " ";
      printRange(r);
    }
  }

  void printRange(LiveRange &r) {
    std::cout << "[";
    printPos(r.from);
    std::cout << ";";
    printPos(r.to);
    std::cout << ")";
  }

  void printPos(LivePos pos) {
    std::cout << pos.num << LivePos::slotName(pos.slot);
  }
};
