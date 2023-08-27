#include <cassert>
#include <ir/IR.h>
#include <map>
#include <utility>

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
  for (Operand *use = chNext; use;) {
    SSAUse &tmp = use->ssaUse();
    use = tmp.chNext;
    tmp.def = nullptr;
    tmp.chPrev = nullptr;
    tmp.chNext = nullptr;
  }
  chNext = nullptr;
}
