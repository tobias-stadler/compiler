#pragma once

#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <vector>

class DynBitSet {
public:
  using word_t = uint64_t;
  static constexpr size_t wordBits = 8 * sizeof(word_t);

  static constexpr size_t numWordsForBits(size_t numBits) {
    return (numBits + wordBits - 1) / wordBits;
  }

  static constexpr size_t wordIdx(size_t i) { return i / wordBits; }
  static constexpr size_t bitIdx(size_t i) { return i % wordBits; }
  static constexpr word_t bitMsk(size_t i) { return word_t(1) << bitIdx(i); }

  DynBitSet() : numBits(0) {}

  DynBitSet(size_t numBits)
      : numBits(numBits), words(numWordsForBits(numBits)) {}

  size_t size() const { return numBits; }

  size_t count() const {
    size_t cnt = 0;
    for (size_t i = 0; i < words.size(); ++i) {
      cnt += std::popcount(words[i]);
    }
    return cnt;
  }

  void set(size_t i) {
    assert(i < numBits);
    words[wordIdx(i)] |= bitMsk(i);
  }
  void clr(size_t i) {
    assert(i < numBits);
    words[wordIdx(i)] &= ~bitMsk(i);
  }
  void set(size_t i, bool v) {
    assert(i < numBits);
    if (v) {
      set(i);
    } else {
      clr(i);
    }
  }

  void clrAll() {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = 0;
    }
  }
  void setAll() {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = ~word_t(0);
    }
    clrUndefined();
  }

  bool tst(size_t i) const { return words[wordIdx(i)] & bitMsk(i); }

  DynBitSet &operator&=(const DynBitSet &o) {
    assert(isBinopCompatible(o));
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] &= o.words[i];
    }
    return *this;
  }

  DynBitSet &operator|=(const DynBitSet &o) {
    assert(isBinopCompatible(o));
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] |= o.words[i];
    }
    return *this;
  }

  void flip() {
    for (size_t i = 0; i < words.size(); ++i) {
      words[i] = ~words[i];
    }
    clrUndefined();
  }

  template <typename CB> void for_each(CB cb) const {
    for (size_t i = 0; i < numBits; ++i) {
      if (tst(i)) {
        cb(i);
      }
    }
  }

private:
  size_t numBits;
  std::vector<word_t> words;

  bool isBinopCompatible(const DynBitSet &o) const {
    return numBits == o.numBits;
  }

  void clrUndefined() {
    if (numBits % wordBits == 0)
      return;
    words[words.size() - 1] &= bitMsk(numBits) - 1;
  }
};

class TriangularBitSet {
public:
  static constexpr size_t idx(size_t a, size_t b) {
    assert(a != b);
    size_t h, l;
    if (a < b) {
      l = a;
      h = b;
    } else {
      l = b;
      h = a;
    }
    return (h * (h - 1)) / 2 + l;
  }

  TriangularBitSet(size_t sz) : bits(sz != 0 ? idx(sz, 0) : 0) {}

  void set(size_t i, size_t j) { bits.set(idx(i, j)); }
  void clr(size_t i, size_t j) { bits.clr(idx(i, j)); }
  void set(size_t i, size_t j, bool v) {
    if (v) {
      set(i, j);
    } else {
      clr(i, j);
    }
  }

  bool tst(size_t i, size_t j) { return bits.tst(idx(i, j)); }

  DynBitSet bits;
};
