#pragma once

#include "ir/IR.h"
#include "ir/InstrBuilder.h"
#include <algorithm>
#include <cassert>
#include <memory>
#include <unordered_map>

// Construct SSA using lazy reaching definitions analysis
// Based on approach by M. Braun et al:
// https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf

class SSARenumberTable {
public:
  Operand *get(SSASymbolId id) {
    auto it = symbols.find(id);
    if (it == symbols.end()) {
      return nullptr;
    }
    return it->second;
  }

  void set(SSASymbolId id, Operand &def) { symbols[id] = &def; }
  void set(SSASymbolId id, Operand *def) {
    if (!def)
      return;
    set(id, *def);
  }

  void setMarked(bool val) { marked = val; }
  void setSealed(bool val) { sealed = val; }

  bool isMarked() { return marked; }
  bool isSealed() { return sealed; }

  static void init(Block &block) {
    assert(!block.userData);
    block.userData = new SSARenumberTable();
  }
  static void destroy(Block &block) {
    delete static_cast<SSARenumberTable *>(block.userData);
    block.userData = nullptr;
  }

  static void destroy(Function &func) {
    for (auto &b : func) {
      destroy(b);
    }
  }

  static SSARenumberTable &getTable(Block &b) {
    if (!b.userData) {
      init(b);
    }
    return *static_cast<SSARenumberTable *>(b.userData);
  }

private:
  std::unordered_map<SSASymbolId, Operand *> symbols;
  bool marked = false;
  bool sealed = true;
};

class SSABuilder {
public:
  static Operand *load(SSASymbolId id, Block &block) {
    auto &renum = SSARenumberTable::getTable(block);
    SSADef &blockSSADef = block.getDef().ssaDef();

    Operand *def = renum.get(id);
    if (def) {
      return def;
    }

    if (!renum.isSealed()) {
      assert(false && "Not sealed");
    }
    unsigned numPreds = blockSSADef.getNumUses();
    if (numPreds == 0) {
      return nullptr;
    }
    if (numPreds == 1) {
      def = load(id, blockSSADef.begin()->getParentBlock());
      renum.set(id, def);
      return def;
    }
    if (renum.isMarked()) {
      /*
      auto phi = InstrBuilder().emitPhi();
      phi.setupOperands(numPreds);
      def = &phi->getOperandUnchecked(0);
      insert
      renum.set(id, def);
      return def;
      */
      assert(false && "Endless recursion");
    } else {
      renum.setMarked(true);
      auto phi = InstrBuilder().buildPhi();
      phi.setupOperands(numPreds);
      block.insertBegin(phi.get());
      def = &phi->getDef();
      renum.set(id, *def);
      for (auto &pred : blockSSADef) {
        Block &predBlock = pred.getParentBlock();
        Operand *predDef = load(id, predBlock);
        assert(predDef);
        if (predDef->ssaDefType() != VoidSSAType::get()) {
          phi.setType(predDef->ssaDefType());
        }
        phi.addPred(*predDef, predBlock);
      }
      assert(phi->getDef().ssaDefType() != VoidSSAType::get());
      renum.setMarked(false);
    }
    return def;
  }

  static void store(SSASymbolId id, Operand &def, Block &block) {
    auto &renum = SSARenumberTable::getTable(block);
    renum.set(id, def);
  }
};
