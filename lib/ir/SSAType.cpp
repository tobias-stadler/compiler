#include "ir/IR.h"
#include <map>

namespace {
std::map<unsigned, IntSSAType> otherIntTypes;
} // namespace

VoidSSAType VoidSSAType::instance;

IntSSAType &IntSSAType::get(unsigned bits) {
  assert(bits > 0);
  auto [it, succ] =
      otherIntTypes.insert(std::make_pair(bits, IntSSAType(bits)));
  return it->second;
}
