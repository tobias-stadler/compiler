#pragma once

#include "ir/IRPass.h"
#include "ir/IRPrinter.h"
#include "ir/InstrBuilder.h"
#include <iostream>
#include <ranges>
#include <unordered_set>

class BlockRegLiveness {
public:
  std::unordered_set<Operand *> liveIn;
  std::unordered_set<Operand *> liveOut;
  std::vector<Operand *> liveOutNew;
  std::unordered_set<Operand *> defs;
};

class RegLiveness {
public:
  static const IRInfoID ID;
  std::unordered_map<Block *, BlockRegLiveness> blockLiveness;
};

class RegLivenessPass : public IRPass<Function> {
public:
  const char *name() override { return "RegLivenessPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    data.blockLiveness.clear();

    std::vector<Block *> workList;
    for (auto &block : func) {
      precomputeBlock(block);
      workList.push_back(&block);
    }
    while (!workList.empty()) {
      dataflowStep(workList);
    }
    info.publish(data);
  }

  void precomputeBlock(Block &block) {
    std::unordered_set<Operand *> exposedDefs;
    auto &blockLive = data.blockLiveness[&block];
    for (auto &instr : block | std::views::reverse) {
      if (instr.isPhi()) {
        auto phi = PhiInstrPtr(&instr);
        for (int i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &predBlockLive = data.blockLiveness[&phi.getPredecessorBlock(i)];
          predBlockLive.liveOutNew.push_back(&phi.getPredecessorDef(i));
        }
        blockLive.liveIn.insert(&instr.getDef());
        exposedDefs.erase(&instr.getDef());
        continue;
      }
      for (auto &op : instr) {
        if (op.isSSAUse()) {
          if (!op.ssaUse().getDef().isSSARegDef())
            continue;
          exposedDefs.insert(&op.ssaUse().getDef());
        } else if (op.isSSADef()) {
          blockLive.defs.insert(&op);
          exposedDefs.erase(&op);
        }
      }
    }
    blockLive.liveIn.insert(exposedDefs.begin(), exposedDefs.end());
    for (auto &blockUse : block.getDef().ssaDef()) {
      Block &pred = blockUse.getParentBlock();
      auto &predBlockLive = data.blockLiveness[&pred];
      for (auto *op : exposedDefs) {
        predBlockLive.liveOutNew.push_back(op);
      }
    }
  }

  void dataflowStep(std::vector<Block *> &workList) {
    auto &block = *workList.back();
    workList.pop_back();
    auto &blockLive = data.blockLiveness[&block];
    for (auto *op : blockLive.liveOutNew) {
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
    info.advertise<RegLiveness>();
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<RegLiveness>();
    data.blockLiveness.clear();
  }

private:
  RegLiveness data;
};

class PrintRegLivenessPass : public IRPass<Function> {
  const char *name() { return "PrintRegLivenessPass"; }

  void run(Function &func, IRInfo<Function> &info) {
    auto regLive = info.query<RegLiveness>();
    auto printer = info.query<PrintIRVisitor>();
    std::cout << "-- RegLiveness --\n";
    for (auto &block : func) {
      auto blockLive = regLive.blockLiveness[&block];
      printer.printNumberedDef(block.getDef());
      std::cout << ":\n";
      std::cout << "LiveIn:";
      for (auto x : blockLive.liveIn) {
        std::cout << " ";
        printer.printNumberedDef(*x);
      }
      std::cout << "\n";
      std::cout << "LiveOut:";
      for (auto x : blockLive.liveOut) {
        std::cout << " ";
        printer.printNumberedDef(*x);
      }
      std::cout << "\n";
    }
    std::cout << "----\n";
  }
};
