#pragma once

#include "ir/IRPass.h"
#include "ir/IRPrinter.h"
#include "ir/InstrBuilder.h"
#include "ir/RegTracking.h"
#include "support/DynBitSet.h"
#include <cassert>
#include <compare>
#include <iostream>
#include <ranges>
#include <unordered_set>

class BlockLiveFlow {
public:
  std::unordered_set<Reg> liveIn;
  std::unordered_set<Reg> liveOut;
  std::vector<Reg> liveOutNew;
  std::unordered_set<Reg> defs;
};

class LiveFlow {
public:
  static const IRInfoID ID;
  std::unordered_map<Block *, BlockLiveFlow> blockLive;
};

class LiveFlowPass : public IRPass<Function> {
public:
  const char *name() override { return "LiveFlowPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    data.blockLive.clear();
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
    auto &blockLive = data.blockLive[&block];
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi()) {
        auto phi = PhiInstrPtr(&instr);
        for (int i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &predBlockLive = data.blockLive[&phi.getPredecessorBlock(i)];
          predBlockLive.liveOutNew.push_back(
              regTrack->getRegForSSADef(phi.getPredecessorDef(i)));
        }
        Reg reg = regTrack->getRegForSSADef(instr.getDef());
        blockLive.liveIn.insert(reg);
        regExposed.erase(reg);
        continue;
      }

      for (auto &op : instr) {
        auto [reg, isDef] = regTrack->getRegForOperand(op);
        if (!reg)
          continue;
        if (isDef) {
          if (ignoreReg(*arch, reg))
            continue;
          blockLive.defs.insert(reg);
          regExposed.erase(reg);
        } else {
          if (ignoreReg(*arch, reg))
            continue;
          regExposed.insert(reg);
        }
      }
    }

    blockLive.liveIn.insert(regExposed.begin(), regExposed.end());
    for (auto &blockUse : block.getDef().ssaDef()) {
      Block &pred = blockUse.getParentBlock();
      auto &predBlockLive = data.blockLive[&pred];
      predBlockLive.liveOutNew.insert(predBlockLive.liveOutNew.end(),
                                      regExposed.begin(), regExposed.end());
    }
  }

  static bool ignoreReg(Arch &arch, Reg reg) {
    if (reg.isVReg())
      return false;
    const ArchReg *archReg = arch.getArchReg(reg);
    return archReg ? archReg->noLiveness : false;
  }

  void dataflowStep(Block &block, std::vector<Block *> &workList) {
    auto &blockLive = data.blockLive[&block];
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
        auto &predBlockLive = data.blockLive[&pred];
        predBlockLive.liveOutNew.push_back(op);
        workList.push_back(&pred);
      }
    }
    blockLive.liveOutNew.clear();
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveFlow>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveFlow>();
    data.blockLive.clear();
  }

private:
  LiveFlow data;
  Arch *arch;
  RegTracking *regTrack;
};

class PrintLiveFlowPass : public IRPass<Function> {
  const char *name() { return "PrintLiveFlowPass"; }

  void run(Function &func, IRInfo<Function> &info) {
    auto regLive = info.query<LiveFlow>();
    auto printer = info.query<PrintIRVisitor>();
    std::cout << "-- LivenessFlow --\n";
    for (auto &block : func) {
      auto blockLive = regLive.blockLive[&block];
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
    intervals = LiveIntervals();
    regTrack = &info.query<RegTracking>();
    flow = &info.query<LiveFlow>();
    numberInstructions(func);
    buildIntervals(func);
    info.publish(intervals);
    info.preserveAll();
  }

  void numberInstructions(Function &func) {
    for (auto &block : func) {
      intervals.number(&block.getSentryBegin());
      for (auto &instr : block) {
        intervals.number(&instr);
      }
      intervals.number(&block.getSentryEnd());
    }
  }

  void buildIntervals(Function &func) {
    for (auto &block : func | std::views::reverse) {
      auto &blockLive = flow->blockLive[&block];
      auto live = blockLive.liveOut;
      auto blockIn = intervals.getNumLiveIn(block);
      auto blockOut = intervals.getNumLiveOut(block);
      for (auto reg : live) {
        intervals.intervals[reg].addRangeCoalesceBegin(blockIn, blockOut);
      }
      for (auto &instr : block | std::views::reverse) {
        if (instr.isPhi())
          continue;
        for (auto &op : instr) {
          auto [reg, isDef] = regTrack->getRegForOperand(op);
          if (!reg)
            continue;
          if (isDef) {
            if (LiveFlowPass::ignoreReg(*arch, reg))
              continue;
            defReg(reg, instr, live);
          } else {
            if (LiveFlowPass::ignoreReg(*arch, reg))
              continue;
            useReg(reg, instr, live);
          }
        }
      }
    }
  }

  void useReg(Reg reg, Instr &instr, std::unordered_set<Reg> &live) {
    auto blockIn = intervals.getNumLiveIn(instr.getParent());
    auto instrNum = intervals.getNumInstr(instr);
    if (live.insert(reg).second) {
      intervals.intervals[reg].addRangeCoalesceBegin(blockIn, instrNum);
    }
  }

  void defReg(Reg reg, Instr &instr, std::unordered_set<Reg> &live) {
    auto instrNum = intervals.getNumInstr(instr);
    if (live.erase(reg)) {
      intervals.intervals[reg].updateFromBegin(instrNum);
    } else {
      intervals.intervals[reg].addRangeCoalesceBegin(instrNum,
                                                     {instrNum, LivePos::LATE});
    }
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveIntervals>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveIntervals>();
  }

  LiveFlow *flow;
  RegTracking *regTrack;
  Arch *arch;
  LiveIntervals intervals;
};

class PrintLiveIntervalsPass : public IRPass<Function> {
  const char *name() override { return "PrintLiveIntervalsPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    LiveIntervals &live = info.query<LiveIntervals>();
    PrintIRVisitor printer(info.getArchUnchecked());
    printer.setPreInstrCallback(
        [&](Instr &i) { std::cout << live.getNumInstr(i) << ": "; });
    printer.dispatch(func);
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

class LiveSet {
public:
  LiveSet() {}
  LiveSet(size_t sz) : bits(sz) {}

  void insert(Reg reg) { bits.set(reg.getIdx()); }

  void erase(Reg reg) { bits.clr(reg.getIdx()); }

  bool contains(Reg reg) { return bits.tst(reg.getIdx()); }

  DynBitSet bits;
};

class LiveGraph {
  friend class LiveGraphPass;
  friend class PrintLiveGraphPass;

public:
  static const IRInfoID ID;

  struct VLive {
    VLive(size_t numPhysRegs) : physLive(numPhysRegs) {}
    LiveSet physLive;
    std::vector<size_t> adjVLive;
    bool ignore = false;
  };

  LiveGraph(size_t numVRegs, size_t numPhysRegs)
      : numVRegs(numVRegs), numPhysRegs(numPhysRegs), adjVLive(numVRegs) {
    vLive.reserve(numVRegs);
    for (size_t i = 0; i < numVRegs; ++i) {
      vLive.emplace_back(numPhysRegs);
    }
  }

  void addEdge(Reg r1, Reg r2) {
    if (r1.isVReg() && r2.isVReg()) {
      addVEdge(r1, r2);
    } else if (r1.isVReg() && r2.isPhysReg()) {
      addPhysEdge(r1, r2);
    } else if (r2.isVReg() && r1.isPhysReg()) {
      addPhysEdge(r2, r1);
    }
  }

  void addVEdge(Reg v1, Reg v2) {
    assert(v1.isVReg() && v2.isVReg());
    if (adjVLive.tst(v1.getIdx(), v2.getIdx())) {
      return;
    }
    adjVLive.set(v1.getIdx(), v2.getIdx());
    getVLive(v1).adjVLive.push_back(v2.getIdx());
    getVLive(v2).adjVLive.push_back(v1.getIdx());
  }

  void addPhysEdge(Reg v, Reg phys) {
    assert(v.isVReg() && phys.isPhysReg());
    getVLive(v).physLive.insert(phys);
  }

private:
  size_t numVRegs;
  size_t numPhysRegs;

  TriBitSet adjVLive;
  std::vector<VLive> vLive;

  VLive &getVLive(Reg reg) {
    assert(reg.isVReg());
    return vLive[reg.getIdx()];
  }
};

class LiveGraphPass : public IRPass<Function> {
  const char *name() override { return "LiveGraphPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    arch = &info.getArch();
    regTrack = &info.query<RegTracking>();
    flow = &info.query<LiveFlow>();
    graph = std::make_unique<LiveGraph>(regTrack->getNumVRegs(),
                                        arch->getArchRegs().size());
    for (auto &block : func) {
      buildGraphForBlock(block);
    }
    info.publish(*graph);
    info.preserveAll();
  }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<LiveGraph>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<LiveGraph>();
    graph = nullptr;
  }

  void buildGraphForBlock(Block &block) {
    auto &blockLive = flow->blockLive[&block];
    auto live = blockLive.liveOut;
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi())
        continue;
      for (auto &op : instr) {
        auto [reg, isDef] = regTrack->getRegForOperand(op);
        if (!reg)
          continue;
        if (isDef) {
          if (LiveFlowPass::ignoreReg(*arch, reg))
            continue;
          // FIXME: should multiple defs always interfere with each other?
          live.erase(reg);
          for (auto oReg : live) {
            graph->addEdge(reg, oReg);
          }
        } else {
          if (LiveFlowPass::ignoreReg(*arch, reg))
            continue;
          live.insert(reg);
        }
      }
    }
  }

private:
  std::unique_ptr<LiveGraph> graph;
  LiveFlow *flow;
  RegTracking *regTrack;
  Arch *arch;
};

class PrintLiveGraphPass : public IRPass<Function> {
  const char *name() override { return "PrintLiveGraphPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    LiveGraph &graph = info.query<LiveGraph>();
    printer = &info.query<PrintIRVisitor>();

    for (size_t i = 0; i < graph.numVRegs; ++i) {
      std::cout << i << ":\n";
      printVLive(graph.vLive[i]);
    }

    info.preserveAll();
  }

  void printVLive(LiveGraph::VLive v) {
    std::cout << "  Phys:";
    v.physLive.bits.for_each([this](auto i) {
      std::cout << " ";
      printer->printReg(i);
    });
    std::cout << "\n";
    std::cout << "  Virt:";
    for (auto i : v.adjVLive) {
      std::cout << " ";
      printer->printReg(Reg::vReg(i));
    }
    std::cout << "\n";
  }

  PrintIRVisitor *printer;
};
