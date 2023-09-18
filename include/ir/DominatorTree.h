#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRPrinter.h"

class DominatorTree {
public:
  static const IRInfoID ID;

  unsigned numBlocks = 0;
  std::unordered_map<Block *, int> dfsPreorderNum;
  std::vector<Block *> dfsPreorderBlock;
  std::vector<int> dfsPreorderParent;
  std::vector<int> forestParent;
  std::vector<int> forestLabel;
  std::vector<int> semiDominator;
  std::vector<int> immDominator;
};

class DominatorTreePass : public IRPass<Function> {

  void advertise(IRInfo<Function> &info) { info.advertise<DominatorTree>(); }

  void run(Function &func, IRInfo<Function> &info) {
    tree = std::make_unique<DominatorTree>();
    dfs(func.getEntry(), -1);
    computeSemiDominators();
    computeImmDominators();
    info.publish(*tree);
  }

  void invalidate(IRInfo<Function> &info) {
    info.retract<DominatorTree>();
    tree = nullptr;
  }

  void dfs(Block &block, int parentId) {
    int id = tree->numBlocks;
    auto [_, succ] = tree->dfsPreorderNum.emplace(&block, id);
    if (!succ)
      return;
    tree->dfsPreorderBlock.push_back(&block);
    tree->dfsPreorderParent.push_back(parentId);
    ++tree->numBlocks;

    Instr &term = static_cast<Instr &>(block.getSentryEnd().getPrevNode());
    switch (term.getKind()) {
    case Instr::INSTR_BR:
      dfs(term.getOperand(0).ssaUse().getDef().ssaDefBlock(), id);
      break;
    case Instr::INSTR_BR_COND:
      dfs(term.getOperand(1).ssaUse().getDef().ssaDefBlock(), id);
      dfs(term.getOperand(2).ssaUse().getDef().ssaDefBlock(), id);
      break;
    }
  }

  void computeSemiDominators() {
    tree->forestParent = std::vector<int>(tree->numBlocks, -1);
    tree->forestLabel = std::vector<int>(tree->numBlocks, tree->numBlocks);
    tree->semiDominator = std::vector<int>(tree->numBlocks, -1);
    for (int w = tree->numBlocks - 1; w > 0; --w) {
      int sd = w;
      for (auto &pred : tree->dfsPreorderBlock[w]->getDef().ssaDef()) {
        int v = tree->dfsPreorderNum[&pred.getParentBlock()];
        if (v <= w) {
          sd = std::min(sd, v);
        } else {
          sd = std::min(sd, eval(v));
        }
      }
      tree->semiDominator[w] = sd;
      link(w);
    }
  }

  void computeImmDominators() {
    tree->immDominator = std::vector<int>(tree->numBlocks, -1);
    for (unsigned w = 1; w < tree->numBlocks; ++w) {
      int candidate = tree->dfsPreorderParent[w];
      int sd = tree->semiDominator[w];
      while (candidate > sd) {
        candidate = tree->immDominator[candidate];
      }
      tree->immDominator[w] = candidate;
    }
  }

  void link(int w) {
    int v = tree->dfsPreorderParent[w];
    assert(tree->forestParent[w] == -1);
    assert(tree->forestParent[v] == -1);
    tree->forestParent[w] = v;
    tree->forestLabel[v] =
        std::min(tree->forestLabel[v], tree->semiDominator[w]);
  }

  int eval(int id) {
    assert(tree->forestParent[id] != -1);
    int root;
    for (int i = id; i != -1; i = tree->forestParent[i]) {
      root = i;
    }
    for (int i = id, parent; (parent = tree->forestParent[i]) != -1;) {
      tree->forestParent[i] = root;
      i = parent;
    }
    return tree->forestLabel[root];
  }

  const char *name() { return "DominatorTreePass"; }

private:
  std::unique_ptr<DominatorTree> tree;
};

class PrintDominatorTreePass : public IRPass<Function> {
  const char *name() { return "PrintDominatorTreePass"; }

  void run(Function &, IRInfo<Function> &info) {
    auto domTree = info.query<DominatorTree>();
    auto printer = info.query<PrintIRVisitor>();
    std::cout << "-- DominatorTree --\n";
    for (unsigned i = 0; i < domTree.numBlocks; ++i) {
      std::cout << "dfs: " << i << ", block: ";
      printer.printNumberedDef(domTree.dfsPreorderBlock[i]->getDef());
      std::cout << ", parent: " << domTree.dfsPreorderParent[i]
                << ", fParent: " << domTree.forestParent[i]
                << ", fLabel: " << domTree.forestLabel[i]
                << ", semiDom: " << domTree.semiDominator[i]
                << ", iDom: " << domTree.immDominator[i] << "\n";
    }
    std::cout << "----\n";
  }
};
