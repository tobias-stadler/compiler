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

  static void init(Block &block) { block.userData = new SSARenumberTable(); }
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
    return *static_cast<SSARenumberTable *>(b.userData);
  }

private:
  std::unordered_map<SSASymbolId, Operand *> symbols;
  bool marked = false;
  bool sealed = true;
};

class SSASymbol {
  enum Storage {
    SSA,
    ALLOCA,
  };
};

class SSABuilder {
public:
  SSABuilder() : program(std::make_unique<Program>()) {}

  Operand *loadSSA(SSASymbolId id, Block &block) {
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
      def = loadSSA(id, blockSSADef.begin()->getParentBlock());
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
      auto phi = instr.buildPhi();
      phi.setupOperands(numPreds);
      block.insertBegin(phi.get());
      renum.set(id, phi->getDef());
      for (auto &pred : blockSSADef) {
        Block &predBlock = pred.getParentBlock();
        Operand *predDef = loadSSA(id, predBlock);
        if (!predDef) {
          continue;
        }
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

  Operand *loadSSA(SSASymbolId id) {
    assert(currBlock);
    return loadSSA(id, *currBlock);
  }

  void storeSSA(SSASymbolId id, Operand &def, Block &block) {
    auto &renum = SSARenumberTable::getTable(block);
    renum.set(id, def);
  }

  void storeSSA(SSASymbolId id, Operand &def) {
    assert(currBlock);
    storeSSA(id, def, *currBlock);
  }

  Function &startFunction() {
    assert(!currFunc);
    auto func = std::make_unique<Function>();
    currFunc = func.get();
    program->addFunction(std::move(func));
    return *currFunc;
  }

  Function &endFunction() {
    assert(currFunc);
    assert(!currBlock);
    Function &func = *currFunc;
    SSARenumberTable::destroy(func);
    currFunc = nullptr;
    return func;
  }

  Block &startBlock() {
    assert(currFunc);
    assert(!currBlock);
    currBlock = new Block();
    SSARenumberTable::init(*currBlock);
    currFunc->insertEnd(currBlock);
    instr.setInsertionPoint(*currBlock);
    return *currBlock;
  }

  Block &endBlock() {
    assert(currBlock);
    Block &block = *currBlock;
    currBlock = nullptr;
    instr.setInsertionPoint(nullptr);
    return block;
  }

  Block &getBlock() {
    assert(currBlock);
    return *currBlock;
  }

  Function &getFunc() {
    assert(currFunc);
    return *currFunc;
  }

  Operand &getDef() {
    assert(instr.getLastInstr());
    return instr.getLastInstr()->getDef();
  }

  std::unique_ptr<Program> finish() { return std::move(program); }

  InstrBuilder &operator*() { return instr; }
  InstrBuilder *operator->() { return &instr; }

  void ensureStackSlot(SSASymbolId id, SSAType &type) {}

  void storeStack(SSASymbolId id) {}

  Operand *loadStack(SSASymbolId id) {}

private:
  std::unique_ptr<Program> program;
  Function *currFunc;
  Block *currBlock;
  InstrBuilder instr;
};
