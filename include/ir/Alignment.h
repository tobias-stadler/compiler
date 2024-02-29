#pragma once

#include <cstddef>

class Alignment {
public:
  constexpr Alignment() : alignExp(0) {}
  constexpr explicit Alignment(unsigned alignExp) : alignExp(alignExp) {}

  constexpr unsigned getExp() { return alignExp; }
  constexpr size_t getSize() { return 1ULL << alignExp; }
  constexpr size_t getMask() { return getSize() - 1; }
  constexpr size_t getInvMask() { return ~getMask(); }

  constexpr size_t alignSize(size_t sz) {
    return (sz + getMask()) & getInvMask();
  }

private:
  unsigned alignExp;
};
