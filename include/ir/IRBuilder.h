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

  Function &createAndSetFunction() {
    auto func = std::make_unique<Function>();
    currFunc = func.get();
    program->addFunction(std::move(func));
    return *currFunc;
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

  Block &createBlock() {
    assert(currFunc);
    Block *b = new Block();
    currFunc->insertEnd(b);
    return *b;
  }

  Block &createAndSetBlock() {
    assert(!currBlock);
    setBlock(createBlock());
    return *currBlock;
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
