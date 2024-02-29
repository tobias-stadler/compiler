#pragma once

#include "ir/SSAType.h"
#include "support/RTTI.h"
#include <cassert>
#include <functional>
#include <iterator>

class Operand;
class Instr;
class Block;

class OtherSSADef;
class GlobalDef;

class Reg {
public:
  friend class std::hash<Reg>;
  using num_t = unsigned;

  constexpr Reg() : num(0) {}

  constexpr Reg(num_t num) : num(num) {}

  constexpr explicit operator bool() const { return num; }

  constexpr operator num_t() const { return num; }

  constexpr num_t getNum() const { return num; }

private:
  num_t num;
};

template <> struct std::hash<Reg> {
  size_t operator()(const Reg &reg) const { return reg.num; }
};

class BrCond {
public:
  enum Kind { EQ, NE, LT, LTU, LE, LEU, GT, GTU, GE, GEU };

  constexpr BrCond(Kind kind) : kind(kind) {}

  static constexpr BrCond eq() { return BrCond(EQ); }
  static constexpr BrCond ne() { return BrCond(NE); }
  static constexpr BrCond lt() { return BrCond(LT); }
  static constexpr BrCond ltu() { return BrCond(LTU); }
  static constexpr BrCond le() { return BrCond(LE); }
  static constexpr BrCond leu() { return BrCond(LEU); }
  static constexpr BrCond gt() { return BrCond(GT); }
  static constexpr BrCond gtu() { return BrCond(GTU); }
  static constexpr BrCond ge() { return BrCond(GE); }
  static constexpr BrCond geu() { return BrCond(GEU); }

  static constexpr const char *kindName(Kind kind) {
    switch (kind) {
    case EQ:
      return "eq";
    case NE:
      return "ne";
    case LT:
      return "lt";
    case LTU:
      return "ltu";
    case LE:
      return "le";
    case LEU:
      return "leu";
    case GT:
      return "gt";
    case GTU:
      return "gtu";
    case GE:
      return "ge";
    case GEU:
      return "geu";
    }
    return "?";
  }

  constexpr BrCond negate() {
    switch (kind) {
    case EQ:
      return BrCond(NE);
    case NE:
      return BrCond(EQ);
    case LT:
      return BrCond(GE);
    case LTU:
      return BrCond(GEU);
    case LE:
      return BrCond(GT);
    case LEU:
      return BrCond(GTU);
    case GT:
      return BrCond(LE);
    case GTU:
      return BrCond(LEU);
    case GE:
      return BrCond(LT);
    case GEU:
      return BrCond(LTU);
    }
  }

  constexpr BrCond commute() {
    switch (kind) {
    case EQ:
    case NE:
      break;
    case LT:
      return BrCond(GT);
    case LTU:
      return BrCond(GTU);
    case LE:
      return BrCond(GE);
    case LEU:
      return BrCond(GEU);
    case GT:
      return BrCond(LT);
    case GTU:
      return BrCond(LTU);
    case GE:
      return BrCond(LE);
    case GEU:
      return BrCond(LEU);
    }
    return *this;
  }

  friend bool operator==(const BrCond &a, const BrCond &b) {
    return a.getKind() == b.getKind();
  }

  Kind getKind() const { return kind; }

  constexpr bool isSigned() {
    switch (kind) {
    case LT:
    case LE:
    case GT:
    case GE:
      return true;
    default:
      return false;
    }
  }

private:
  Kind kind;
};

class SSAUse {
  friend class SSADef;
  friend class Operand;

public:
  SSAUse() {}
  SSAUse(const SSAUse &o) = delete;
  SSAUse &operator=(const SSAUse &o) = delete;
  ~SSAUse() { unlink(); }
  Operand &getDef() const {
    assert(def);
    return *def;
  }
  Instr &getDefInstr() const;
  void unlink();

private:
  Operand *def = nullptr;
  Operand *chNext = nullptr;
  Operand *chPrev = nullptr;
};

class SSADef {
  friend class SSAUse;
  friend class Operand;

public:
  SSADef(SSAType &type) : contentType(&type) {}
  SSADef(SSAType *type) : contentType(type) {}

  SSADef(const SSADef &o) = delete;
  SSADef &operator=(const SSADef &o) = delete;
  ~SSADef() { unlinkAllUses(); }

  void unlinkAllUses();
  void replaceAllUses(Operand &newDef);
  void replaceAllUses(Reg reg);

  class iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = Operand;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;

    iterator() {}
    iterator(Operand *ref) : mPtr(ref) {}

    reference operator*() const { return *mPtr; }
    pointer operator->() const { return mPtr; }

    iterator &operator++();

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.mPtr == b.mPtr;
    }

    friend bool operator!=(const iterator &a, const iterator &b) {
      return a.mPtr != b.mPtr;
    }

  private:
    Operand *mPtr = nullptr;
  };

  static_assert(std::forward_iterator<iterator>);

  iterator begin() { return chNext; }
  iterator end() { return nullptr; }

  bool hasExactlyNUses(unsigned n);
  bool hasUses() { return chNext; }
  unsigned getNumUses() const;

private:
  Operand *chNext = nullptr;
  SSAType *contentType;
};

class OperandChain {
public:
  OperandChain() {}
  OperandChain(const OperandChain &o) = delete;
  OperandChain &operator=(const OperandChain &o) = delete;
  ~OperandChain() { deallocate(); }

  using iterator = Operand *;
  iterator begin();
  iterator end();

  void allocate(unsigned cap);
  void deallocate();
  Operand &getOperand(unsigned n);
  unsigned getCapacity() { return capacity; }
  bool isAllocated() { return operands; }

private:
  unsigned capacity = 0;
  Operand *operands = nullptr;
};

class Operand {
  friend class Instr;

public:
  enum Kind {
    EMPTY,
    SSA_DEF_TYPE,
    SSA_DEF_OTHER,
    SSA_USE,
    REG_DEF,
    REG_USE,
    BLOCK,
    TYPE,
    IMM32,
    BRCOND,
    CHAIN,
  };
  static constexpr bool kindIsSSADef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_OTHER:
      return true;
    default:
      return false;
    }
  }
  static constexpr bool kindIsSSARegDef(Kind kind) {
    return kind == SSA_DEF_TYPE;
  }
  static constexpr bool kindIsSSAUse(Kind kind) { return kind == SSA_USE; }
  static constexpr bool kindIsReg(Kind kind) {
    return kind == REG_DEF || kind == REG_USE;
  }
  static constexpr bool kindIsDef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_OTHER:
    case REG_DEF:
      return true;
    default:
      return false;
    }
  }

  Operand() {}
  Operand(const Operand &o) = delete;
  Operand(Operand &&o) = delete;
  Operand &operator=(Operand &&o) = delete;
  Operand &operator=(const Operand &o) {
    destroy();
    switch (o.kind) {
    case EMPTY:
      break;
    case SSA_DEF_TYPE:
    case SSA_DEF_OTHER:
      assert(false && "Cannot copy SSA operand");
      break;
    case SSA_USE:
      emplaceRaw<SSA_USE>();
      o.ssaUse().getDef().ssaDefAddUse(*this);
      break;
    case REG_DEF:
      assert(false && "Cannot copy reg def");
      break;
    case REG_USE:
      emplaceRaw<REG_USE>(o.contentReg);
      break;
    case BLOCK:
      emplaceRaw<BLOCK>(o.contentBlock);
      break;
    case TYPE:
      emplaceRaw<TYPE>(o.contentType);
      break;
    case IMM32:
      emplaceRaw<IMM32>(o.contentImm32);
      break;
    case BRCOND:
      emplaceRaw<BRCOND>(o.contentBrCond);
      break;
    case CHAIN:
      assert(false && "Don't copy chain!");
      break;
    }
    return *this;
  }
  ~Operand() { destroy(); }

  Kind getKind() { return kind; }

  void destroy() {
    switch (kind) {
    case EMPTY:
    case BLOCK:
    case TYPE:
    case IMM32:
      break;
    case REG_DEF:
    case REG_USE:
      contentReg.~Reg();
      break;
    case SSA_DEF_TYPE:
    case SSA_DEF_OTHER:
      contentDef.~SSADef();
      break;
    case SSA_USE:
      contentUse.~SSAUse();
      break;
    case CHAIN:
      contentChain.~OperandChain();
      break;
    case BRCOND:
      contentBrCond.~BrCond();
      break;
    }
    kind = EMPTY;
  }

  template <Kind K, typename... ARGS> void emplaceRaw(ARGS &&...args) {
    destroy();
    if constexpr (kindIsSSADef(K)) {
      new (&contentDef) SSADef(std::forward<ARGS>(args)...);
    } else if constexpr (K == SSA_USE) {
      new (&contentUse) SSAUse(std::forward<ARGS>(args)...);
    } else if constexpr (K == REG_DEF || K == REG_USE) {
      new (&contentReg) Reg(std::forward<ARGS>(args)...);
    } else if constexpr (K == IMM32) {
      new (&contentImm32) int32_t(std::forward<ARGS>(args)...);
    } else if constexpr (K == BRCOND) {
      new (&contentBrCond) BrCond(std::forward<ARGS>(args)...);
    } else if constexpr (K == BLOCK) {
      new (&contentBlock) Block *(std::forward<ARGS>(args)...);
    } else if constexpr (K == TYPE) {
      new (&contentType) SSAType *(std::forward<ARGS>(args)...);
    } else if constexpr (K == EMPTY) {
    } else if constexpr (K == CHAIN) {
      new (&contentChain) OperandChain(std::forward<ARGS>(args)...);
    } else {
      static_assert(false, "Operand kind cannot be emplaced with these args");
    }
    kind = K;
  }

  template <Kind K, typename... ARGS>
  void emplace(Instr *parent, ARGS &&...args) {
    emplaceRaw<K>(std::forward<ARGS>(args)...);
    this->parent = parent;
  }

  template <Kind K>
  std::enable_if_t<K == SSA_USE> emplace(Instr *parent, Operand &defOp) {
    emplace<SSA_USE>(parent);
    defOp.ssaDefAddUse(*this);
  }

  SSADef &ssaDef() {
    assert(kindIsSSADef(kind));
    return contentDef;
  }

  SSAType &ssaDefType() {
    assert(kind == SSA_DEF_TYPE);
    return *ssaDef().contentType;
  }

  void ssaDefSetType(SSAType &type) {
    assert(kind == SSA_DEF_TYPE);
    ssaDef().contentType = &type;
  }

  Block &ssaDefBlock() { return as<Block>(ssaDefOther()); }

  OtherSSADef &ssaDefOther() {
    assert(kind == SSA_DEF_OTHER);
    return reinterpret_cast<OtherSSADef &>(*this);
  }

  void ssaDefReplace(Reg reg) {
    ssaDef().replaceAllUses(reg);
    emplaceRaw<REG_DEF>(reg);
  }

  Block &block() const {
    assert(kind == BLOCK && contentBlock);
    return *contentBlock;
  }

  SSAType &type() const {
    assert(kind == TYPE && contentType);
    return *contentType;
  }

  BrCond brCond() const {
    assert(kind == BRCOND);
    return contentBrCond;
  }

  OperandChain &chain() {
    assert(kind == CHAIN);
    return contentChain;
  }
  const OperandChain &chain() const {
    assert(kind == CHAIN);
    return contentChain;
  }

  void ssaDefAddUse(Operand &useOp) {
    SSADef &def = ssaDef();
    SSAUse &use = useOp.ssaUse();
    assert(!use.def);
    use.chNext = def.chNext;
    use.chPrev = nullptr;
    use.def = this;

    if (def.chNext) {
      def.chNext->ssaUse().chPrev = &useOp;
    }

    def.chNext = &useOp;
  }

  void ssaUseReplace(Operand &newDef) {
    SSAUse &use = ssaUse();
    use.unlink();
    newDef.ssaDefAddUse(*this);
  }

  SSAUse &ssaUse() {
    assert(kind == SSA_USE);
    return contentUse;
  }
  const SSAUse &ssaUse() const {
    assert(kind == SSA_USE);
    return contentUse;
  }

  int32_t imm32() const {
    assert(kind == IMM32);
    return contentImm32;
  }

  Reg reg() const {
    assert(kindIsReg(kind));
    return contentReg;
  }

  void setParent(Instr *instr) { parent = instr; }

  Instr &getParent() const {
    assert(parent);
    return *parent;
  }

  Block &getParentBlock() const;

  bool isReg() const { return kindIsReg(kind); }
  bool isRegDef() const { return kind == REG_DEF; }
  bool isRegUse() const { return kind == REG_USE; }
  bool isSSADef() const { return kindIsSSADef(kind); }
  bool isSSARegDef() const { return kindIsSSARegDef(kind); }
  bool isSSAUse() const { return kindIsSSAUse(kind); }
  bool isSSARegUse() const {
    return isSSAUse() && ssaUse().getDef().isSSARegDef();
  }

  bool isImplicit() const { return flags.isImplicit; }
  void setImplicit(bool v) { flags.isImplicit = v; }

private:
  Kind kind = EMPTY;
  struct {
    unsigned isImplicit : 1 = 0;
  } flags;
  Instr *parent = nullptr;
  union {
    SSADef contentDef;
    SSAUse contentUse;
    OperandChain contentChain;
    Reg contentReg;
    int32_t contentImm32;
    Block *contentBlock;
    SSAType *contentType;
    BrCond contentBrCond;
  };
};

class OtherSSADef {
public:
  enum Kind {
    GLOBAL_START,
    GLOBAL_FUNCTION,
    GLOBAL_STATIC_MEMORY,
    GLOBAL_END,
    LOCAL_START,
    LOCAL_BLOCK,
    LOCAL_PARAM,
    LOCAL_FRAME,
    LOCAL_MEMORY_ACCESS,
    LOCAL_END
  };

  OtherSSADef(Kind kind) : kind(kind) {
    def.emplace<Operand::SSA_DEF_OTHER>(nullptr, nullptr);
  }
  OtherSSADef(Kind kind, SSAType &type) : kind(kind) {
    def.emplace<Operand::SSA_DEF_OTHER>(nullptr, type);
  }

  Kind getKind() const { return kind; }

  Operand &getDef() { return def; }

  static constexpr bool kindIsGlobal(Kind kind) {
    return kind < GLOBAL_END && kind > GLOBAL_START;
  }

  static constexpr bool kindIsLocal(Kind kind) {
    return kind < LOCAL_END && kind > LOCAL_START;
  }

  bool isGlobal() { return kindIsGlobal(kind); }

  bool isLocal() { return kindIsLocal(kind); }

  GlobalDef &global() { return as<GlobalDef>(*this); }

  Block &block() { return as<Block>(*this); }

protected:
  ~OtherSSADef() = default;

private:
  Operand def; // This needs to be the first member, so that Operand*
               // can be directly converted to OtherSSADef*
  Kind kind;
};

static_assert(std::is_standard_layout_v<OtherSSADef>);

inline SSADef::iterator &SSADef::iterator::operator++() {
  assert(mPtr);
  mPtr = mPtr->ssaUse().chNext;
  return *this;
}
