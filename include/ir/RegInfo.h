#pragma once

#include "ir/Operand.h"
#include <cassert>
#include <memory>

class RegDefUseRoot : public ExternSSADef {
  friend class RegDefUse;
  friend class RegInfo;

public:
  static bool is_impl(const ExternSSADef &o) { return o.getKind() == FUNC_REG; }
  RegDefUseRoot() : ExternSSADef(FUNC_REG) { assert(defUseChain().isLinked()); }

  auto def_begin() {
    return ++DefUseChain::iterator<true, false>(&defUseChain());
  }
  auto def_end() { return DefUseChain::iterator<true, false>(nullptr); }

  auto use_begin() {
    return ++DefUseChain::iterator<false, true>(&defUseChain());
  }
  auto use_end() { return DefUseChain::iterator<false, true>(nullptr); }

  auto defs() { return Range{def_begin(), def_end()}; }
  auto uses() { return Range{use_begin(), use_end()}; }

  auto begin() { return DefUseChain::iterator<>(defUseChain().chNext); }
  auto end() { return DefUseChain::iterator<>(&defUseChain()); }

  size_t count() { return defUseChain().count(); }
  bool empty() { return defUseChain().empty(); }

  size_t count_defs() {
    size_t cnt = 0;
    for (auto &op : defs()) {
      ++cnt;
    }
    return cnt;
  }

  size_t count_uses() {
    size_t cnt = 0;
    for (auto &op : uses()) {
      ++cnt;
    }
    return cnt;
  }

  Operand *getSingleDef() {
    auto *def = defUseChain().chPrev;
    // First CCW node must be a def
    if (!def->operand().isRegDef()) {
      return nullptr;
    }
    // Second CCW node must not be a def
    if (def->chPrev->operand().isRegDef()) {
      return nullptr;
    }
    return &def->operand();
  }

  void replace(Reg reg) {
    assert(defUseChain().isLinked());
    for (DefUseChain *curr = defUseChain().chNext; curr != &defUseChain();) {
      DefUseChain &tmp = *curr;
      curr = curr->chNext;
      // TODO: direct defUse chain splicing
      tmp.operand().regDefUse().replace(reg);
    }
    assert(empty());
  }

private:
  SSAType *ssaType = nullptr;
  unsigned regClass = 0;

  DefUseChain &defUseChain() { return operand().ssaDef(); }
};

template <typename T> class VRegMap {
public:
  using idx_t = Reg::num_t;
  VRegMap() {}
  VRegMap(size_t sz) : data(sz) {}

  T &operator[](Reg::num_t idx) {
    assert(idx < data.size());
    return data[idx];
  }

  T &operator[](Reg idx) {
    assert(idx.isVReg());
    return operator[](idx.getIdx());
  }

  std::vector<T> data;

  class iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = std::pair<Reg, T &>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;
    using It = std::vector<Reg>::iterator;

    iterator() {}
    iterator(T *data, idx_t idx) : data(data), idx(idx) {}

    value_type operator*() const { return {Reg::vReg(idx), data[idx]}; }

    iterator &operator++() {
      ++idx;
      return *this;
    }

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.idx == b.idx;
    }

  private:
    T *data;
    idx_t idx;
  };
  static_assert(std::forward_iterator<iterator>);

  auto begin() { return iterator(data.data(), 0); }
  auto end() { return iterator(data.data(), data.size()); }

private:
};

class RegInfo {
  friend class RegDefUse;

public:
  size_t getNumVRegs() { return vRegs.data.size(); }

  Reg createVReg(SSAType &type) {
    auto [reg, newRoot] = createDefUseRoot();
    newRoot.ssaType = &type;
    return reg;
  }

  Reg cloneVReg(Reg reg) {
    auto [newReg, newRoot] = createDefUseRoot();
    auto &oldRoot = defUseRoot(reg);
    newRoot.ssaType = oldRoot.ssaType;
    newRoot.regClass = oldRoot.regClass;
    return newReg;
  }

  bool isValidVReg(Reg reg) {
    return reg.isVReg() && reg.getIdx() < vRegs.data.size();
  }

  RegDefUseRoot &defUseRoot(Reg reg) { return *vRegs[reg]; }

private:
  std::pair<Reg, RegDefUseRoot &> createDefUseRoot() {
    auto reg = Reg::vReg(vRegs.data.size());
    return {reg, *vRegs.data.emplace_back(std::make_unique<RegDefUseRoot>())};
  }

  VRegMap<std::unique_ptr<RegDefUseRoot>> vRegs;
};
