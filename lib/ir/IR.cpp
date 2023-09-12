#include <cassert>
#include <ir/IR.h>
#include <map>
#include <utility>

namespace {
std::map<unsigned, IntSSAType> otherIntTypes;
} // namespace

VoidSSAType VoidSSAType::instance;
PtrSSAType PtrSSAType::instance;

IntSSAType &IntSSAType::get(unsigned bits) {
  assert(bits > 0);
  auto [it, succ] =
      otherIntTypes.insert(std::make_pair(bits, IntSSAType(bits)));
  return it->second;
}

void SSADef::unlinkAllUses() {
  for (Operand *use = chNext; use;) {
    SSAUse &tmp = use->ssaUse();
    use = tmp.chNext;
    tmp.def = nullptr;
    tmp.chPrev = nullptr;
    tmp.chNext = nullptr;
  }
  chNext = nullptr;
}

void SSAUse::unlink() {
  if (chPrev) {
    chPrev->ssaUse().chNext = chNext;
  } else if (def) {
    def->ssaDef().chNext = chNext;
  }
  if (chNext) {
    chNext->ssaUse().chPrev = chPrev;
  }
  def = nullptr;
  chNext = nullptr;
  chPrev = nullptr;
}

void SSADef::replaceAllUses(Operand &newDef) {
  newDef.ssaDef().chNext = chNext;
  for (Operand *use = chNext; use; use = use->ssaUse().chNext) {
    use->ssaUse().def = &newDef;
  }
  chNext = nullptr;
}

bool SSADef::hasExactlyNUses(unsigned n) {
  unsigned count = 0;
  for (Operand *use = chNext; use && count <= n; use = use->ssaUse().chNext) {
    ++count;
  }
  return count == n;
}

Block &Operand::getParentBlock() { return getParent().getParent(); }

unsigned SSADef::getNumUses() {
  unsigned count = 0;
  for (Operand *use = chNext; use; use = use->ssaUse().chNext) {
    ++count;
  }
  return count;
}

void OperandChain::allocate(unsigned cap) {
  assert(!operands);
  operands = new Operand[cap];
  capacity = cap;
}

void OperandChain::deallocate() {
  delete[] operands;
  capacity = 0;
}
