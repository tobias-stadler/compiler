#pragma once

#include "ir/IRPass.h"
#include "ir/IRPrinter.h"
#include "ir/InstrBuilder.h"
#include <cassert>
#include <compare>
#include <iostream>
#include <ranges>
#include <unordered_set>

template <typename T> class BlockLiveness {
public:
  std::unordered_set<T> liveIn;
  std::unordered_set<T> liveOut;
  std::vector<T> liveOutNew;
  std::unordered_set<T> defs;
};

class BlockLivenessFlow {
public:
  BlockLiveness<Operand *> ssa;
  BlockLiveness<Reg> reg;

  template <typename T> BlockLiveness<T> &get() {
    if constexpr (std::is_same_v<T, Operand *>) {
      return ssa;
    }
    if constexpr (std::is_same_v<T, Reg>) {
      return reg;
    }
  }
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

    std::vector<Block *> workList;
    for (auto &block : func) {
      precomputeBlock(block);
      workList.push_back(&block);
    }
    while (!workList.empty()) {
      auto &block = *workList.back();
      workList.pop_back();
      dataflowStep<Operand *>(block, workList);
      dataflowStep<Reg>(block, workList);
    }
    info.publish(data);
    info.preserveAll();
  }

  void precomputeBlock(Block &block) {
    std::unordered_set<Operand *> ssaExposedDefs;
    std::unordered_set<Reg> regExposed;
    auto &blockLive = data.blockLiveness[&block];
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi()) {
        auto phi = PhiInstrPtr(&instr);
        for (int i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &predBlockLive = data.blockLiveness[&phi.getPredecessorBlock(i)];
          predBlockLive.ssa.liveOutNew.push_back(&phi.getPredecessorDef(i));
        }
        blockLive.ssa.liveIn.insert(&instr.getDef());
        ssaExposedDefs.erase(&instr.getDef());
        continue;
      }
      for (auto &op : instr) {
        if (op.isSSARegUse()) {
          ssaExposedDefs.insert(&op.ssaUse().getDef());
        } else if (op.isSSARegDef()) {
          blockLive.ssa.defs.insert(&op);
          ssaExposedDefs.erase(&op);
        } else if (op.isRegUse()) {
          if (ignoreReg(*arch, op.reg()))
            continue;
          regExposed.insert(op.reg());
        } else if (op.isRegDef()) {
          if (ignoreReg(*arch, op.reg()))
            continue;
          blockLive.reg.defs.insert(op.reg());
          regExposed.erase(op.reg());
        }
      }
    }
    blockLive.ssa.liveIn.insert(ssaExposedDefs.begin(), ssaExposedDefs.end());
    blockLive.reg.liveIn.insert(regExposed.begin(), regExposed.end());
    for (auto &blockUse : block.getDef().ssaDef()) {
      Block &pred = blockUse.getParentBlock();
      auto &predBlockLive = data.blockLiveness[&pred];
      predBlockLive.ssa.liveOutNew.insert(predBlockLive.ssa.liveOutNew.end(),
                                          ssaExposedDefs.begin(),
                                          ssaExposedDefs.end());
      predBlockLive.reg.liveOutNew.insert(predBlockLive.reg.liveOutNew.end(),
                                          regExposed.begin(), regExposed.end());
    }
  }

  static bool ignoreReg(Arch &arch, Reg reg) {
    const ArchReg *archReg = arch.getArchReg(reg);
    return archReg ? archReg->noLiveness : false;
  }

  template <typename T>
  void dataflowStep(Block &block, std::vector<Block *> &workList) {
    auto &blockLive = data.blockLiveness[&block].get<T>();
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
        auto &predBlockLive = data.blockLiveness[&pred].get<T>();
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

      std::cout << "SSALiveIn:";
      print(blockLive.ssa.liveIn, printer);
      std::cout << "SSALiveOut:";
      print(blockLive.ssa.liveOut, printer);

      std::cout << "RegLiveIn:";
      print(blockLive.reg.liveIn, printer);
      std::cout << "RegLiveOut:";
      print(blockLive.reg.liveOut, printer);
    }
    std::cout << "----\n";
    info.preserveAll();
  }

  void print(std::unordered_set<Operand *> ops, PrintIRVisitor &printer) {
    for (auto x : ops) {
      std::cout << " ";
      printer.printNumberedDef(*x);
    }
    std::cout << "\n";
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
  std::unordered_map<Operand *, LiveInterval> ssaIntervals;
  std::unordered_map<Reg, LiveInterval> regIntervals;

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
      auto ssaLive = blockLive.ssa.liveOut;
      auto regLive = blockLive.reg.liveOut;
      auto blockIn = data.getNumLiveIn(block);
      auto blockOut = data.getNumLiveOut(block);
      for (auto reg : ssaLive) {
        data.ssaIntervals[reg].addRangeCoalesceBegin(blockIn, blockOut);
      }
      for (auto reg : regLive) {
        data.regIntervals[reg].addRangeCoalesceBegin(blockIn, blockOut);
      }
      for (auto &instr : block | std::views::reverse) {
        if (instr.isPhi())
          continue;
        for (auto &op : instr) {
          auto instrNum = data.getNumInstr(instr);
          if (op.isSSARegUse()) {
            if (ssaLive.insert(&op.ssaUse().getDef()).second) {
              data.ssaIntervals[&op.ssaUse().getDef()].addRangeCoalesceBegin(
                  blockIn, instrNum);
            }
          } else if (op.isSSARegDef()) {
            if (ssaLive.erase(&op)) {
              data.ssaIntervals[&op].updateFromBegin(instrNum);
            } else {
              data.ssaIntervals[&op].addRangeCoalesceBegin(
                  instrNum, {instrNum, LivePos::LATE});
            }
          } else if (op.isRegUse()) {
            if (LivenessFlowPass::ignoreReg(*arch, op.reg()))
              continue;
            if (regLive.insert(op.reg()).second) {
              data.regIntervals[op.reg()].addRangeCoalesceBegin(blockIn,
                                                                instrNum);
            }
          } else if (op.isRegDef()) {
            if (LivenessFlowPass::ignoreReg(*arch, op.reg()))
              continue;
            if (regLive.erase(op.reg())) {
              data.regIntervals[op.reg()].updateFromBegin(instrNum);
            } else {
              data.regIntervals[op.reg()].addRangeCoalesceBegin(
                  instrNum, {instrNum, LivePos::LATE});
            }
          }
        }
      }
    }
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveIntervals>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveIntervals>();
  }

  LivenessFlow *flow;
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
    for (auto &[reg, interval] : live.ssaIntervals) {
      printer.printNumberedDef(*reg);
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
