#pragma once

#include <cstddef>

class Alignment {
public:
  Alignment() : alignExp(0) {}
  explicit Alignment(unsigned alignExp) : alignExp(alignExp) {}

  unsigned getExp() { return alignExp; }
  size_t getSize() { return 1ULL << alignExp; }
  size_t getMask() { return getSize() - 1; }
  size_t getInvMask() { return ~getMask(); }

  size_t alignSize(size_t sz) { return (sz + getMask()) & getInvMask(); }

private:
  unsigned alignExp;
};
