#pragma once

#include "ir/IR.h"
#include "ir/InstrBuilder.h"
#include <cassert>

class IRBuilder {
public:
  Program &startProgram() {
    assert(!program);
    program = std::make_unique<Program>();
    return *program;
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
    currFunc = nullptr;
    return func;
  }

  void setBlock(Block &block) { setBlock(&block); }

  void setBlock(Block *block) {
    if (block) {
      assert(&block->getParent() == currFunc);
      instr.setInsertionPoint(*block);
    } else {
      instr.setInsertionPoint(nullptr);
    }
    currBlock = block;
  }

  Block &startBlock() {
    assert(currFunc);
    assert(!currBlock);
    currBlock = new Block();
    currFunc->insertEnd(currBlock);
    setBlock(*currBlock);
    return *currBlock;
  }

  Block &endBlock() {
    assert(currBlock);
    Block &block = *currBlock;
    setBlock(nullptr);
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

  std::unique_ptr<Program> endProgram() { return std::move(program); }

  InstrBuilder &operator*() { return instr; }
  InstrBuilder *operator->() { return &instr; }

private:
  std::unique_ptr<Program> program;
  Function *currFunc;
  Block *currBlock;
  InstrBuilder instr;
};
