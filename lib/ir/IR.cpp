#include "ir/IR.h"
#include <cassert>
#include <utility>

Block &Operand::getParentBlock() const { return getParent().getParent(); }

void OperandChain::allocate(unsigned cap) {
  assert(!operands);
  operands = new Operand[cap];
  capacity = cap;
}

void OperandChain::deallocate() {
  delete[] operands;
  capacity = 0;
}

Operand &OperandChain::getOperand(unsigned n) {
  assert(n < capacity);
  return operands[n];
}

OperandChain::iterator OperandChain::begin() { return operands; }

OperandChain::iterator OperandChain::end() { return operands + capacity; }

IntrusiveListNode<Instr, Block> &Block::getFirstNonPhiSentry() {
  for (auto &instr : *this) {
    if (!instr.isPhi()) {
      return instr;
    }
  }
  return getSentryEnd();
}

Function *Program::getFunction(std::string_view name) {
  auto it = functionIndex.find(name);
  if (it == functionIndex.end()) {
    return nullptr;
  }
  return it->second;
}

Function *Program::createFunction(std::string name) {
  auto func = std::make_unique<Function>(std::move(name));
  auto [it, succ] = functionIndex.try_emplace(func->getName(), func.get());
  if (!succ) {
    return nullptr;
  }
  return functionStorage.emplace_back(std::move(func)).get();
}

void RegDefUse::insertDefUse() {
  if (!contentReg.isVReg()) {
    linkSelf();
    return;
  }
  RegDefUseRoot &root =
      operand().getParentBlock().getParent().getRegInfo().defUseRoot(
          contentReg);
  DefUseChain &chainRoot = root.operand().ssaDef();
  if (operand().isRegDef()) {
    chainRoot.insertPrev(*this);
  } else {
    chainRoot.insertNext(*this);
  }
}

void SSADef::replaceAllUses(Reg reg) {
  for (DefUseChain *curr = chNext; curr != this;) {
    DefUseChain &tmp = *curr;
    curr = curr->chNext;
    // TODO: direct conversion without UB!
    // tmp.contentReg = reg;
    // tmp.operand().kind = Operand::REG_USE;
    tmp.operand().data.emplace<Operand::REG_USE>(reg);
  }
}

void SSADef::replace(Reg reg) {
  replaceAllUses(reg);
  operand().data.emplace<Operand::REG_DEF>(reg);
}
