#include <cassert>
#include <ir/IR.h>
#include <map>
#include <utility>

void SSAUse::unlink() {
  if (chPrev) {
    chPrev->chNext = chNext;
  } else if (def) {
    def->chNext = chNext;
  }
  if (chNext) {
    chNext->chPrev = chPrev;
  }
  def = nullptr;
  chNext = nullptr;
  chPrev = nullptr;
}

void SSADef::addUse(SSAUse &use) {
  use.chNext = chNext;
  use.chPrev = nullptr;
  use.def = this;

  if (chNext) {
    chNext->chPrev = &use;
  }

  chNext = &use;
}

SSAUse::SSAUse(Instr &parent, SSADef &def) : parent(&parent) {
  def.addUse(*this);
}

namespace {
std::map<unsigned, IntSSAType> otherIntTypes;
} // namespace

IntSSAType &IntSSAType::get(unsigned bits) {
  assert(bits > 0);
  auto [it, succ] =
      otherIntTypes.insert(std::make_pair(bits, IntSSAType(bits)));
  return it->second;
}

void SSADef::unlinkAllUses() {
  for (SSAUse *use = chNext; use;) {
    SSAUse &tmp = *use;
    use = tmp.chNext;
    tmp.def = nullptr;
    tmp.chPrev = nullptr;
    tmp.chNext = nullptr;
  }
  chNext = nullptr;
}
