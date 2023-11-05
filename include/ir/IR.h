#pragma once

#include "support/IntrusiveList.h"
#include <cassert>
#include <cstdint>
#include <iterator>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>
#include <vector>

using SSASymbolId = unsigned;

class SSAType {
public:
  enum Kind {
    VOID,
    INT,
    PTR,
  };
  static constexpr const char *kindName(Kind kind) {
    switch (kind) {
    case VOID:
      return "void";
    case INT:
      return "i";
    case PTR:
      return "ptr";
    default:
      return "?";
    }
  }

  friend bool operator==(const SSAType &a, const SSAType &b) {
    return &a == &b;
  }
  friend bool operator!=(const SSAType &a, const SSAType &b) {
    return &a != &b;
  }

  Kind getKind() { return kind; }

protected:
  SSAType(Kind kind) : kind(kind) {}
  ~SSAType() {}

private:
  Kind kind;
};

class VoidSSAType : public SSAType {
public:
  static VoidSSAType &get() { return instance; }

private:
  VoidSSAType() : SSAType(VOID) {}
  static VoidSSAType instance;
};

class IntSSAType : public SSAType {
public:
  static IntSSAType &get(unsigned bits);

  unsigned getBits() { return bits; }

private:
  IntSSAType(unsigned bits) : SSAType(INT), bits(bits) {}
  unsigned bits;
};

class PtrSSAType : public SSAType {
public:
  static PtrSSAType &get() { return instance; }

private:
  PtrSSAType() : SSAType(PTR) {}
  static PtrSSAType instance;
};

class Instr;
class Block;
class Function;
class Operand;
class Program;

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

  constexpr BrCond invert() {
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

  friend bool operator==(const BrCond &a, const BrCond &b) {
    return a.getKind() == b.getKind();
  }

  Kind getKind() const { return kind; }

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
  SSADef(unsigned regClass) : contentRegClass(regClass) {}
  SSADef(Block &block) : contentBlock(&block) {}
  SSADef(Block *block) : contentBlock(block) {}
  SSADef(Function &func) : contentFunction(&func) {}
  SSADef(Function *func) : contentFunction(func) {}

  SSADef(const SSADef &o) = delete;
  SSADef &operator=(const SSADef &o) = delete;
  ~SSADef() { unlinkAllUses(); }

  void unlinkAllUses();
  void replaceAllUses(Operand &newDef);

  class iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = Operand;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;

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
    Operand *mPtr;
  };

  iterator begin() { return chNext; }
  iterator end() { return nullptr; }

  bool hasExactlyNUses(unsigned n);
  bool hasUses() { return chNext; }
  unsigned getNumUses() const;

private:
  Operand *chNext = nullptr;
  union {
    SSAType *contentType;
    unsigned contentRegClass;
    Block *contentBlock;
    Function *contentFunction;
  };
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
    SSA_DEF_REGCLASS,
    SSA_DEF_BLOCK,
    SSA_DEF_FUNCTION,
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
    case SSA_DEF_REGCLASS:
    case SSA_DEF_BLOCK:
    case SSA_DEF_FUNCTION:
      return true;
    default:
      return false;
    }
  }
  static constexpr bool kindIsSSARegDef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
      return true;
    default:
      return false;
    }
  }
  static constexpr bool kindIsSSAUse(Kind kind) { return kind == SSA_USE; }
  static constexpr bool kindIsDef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
    case SSA_DEF_BLOCK:
    case SSA_DEF_FUNCTION:
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
    case SSA_DEF_REGCLASS:
    case SSA_DEF_BLOCK:
    case SSA_DEF_FUNCTION:
      assert(false && "Cannot copy SSA Def");
      break;
    case SSA_USE:
      if (o.ssaUse().def) {
        emplace<SSA_USE>(o.parent, o.ssaUse().getDef());
      }
      break;
    case REG_DEF:
      emplace<REG_DEF>(o.parent, o.contentReg);
      break;
    case REG_USE:
      emplace<REG_USE>(o.parent, o.contentReg);
      break;
    case BLOCK:
      emplace<BLOCK>(o.parent, o.contentBlock);
      break;
    case TYPE:
      emplace<TYPE>(o.parent, o.contentType);
      break;
    case IMM32:
      emplace<IMM32>(o.parent, o.contentImm32);
      break;
    case BRCOND:
      emplace<BRCOND>(o.parent, o.contentBrCond);
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
    default:
      break;
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
    case SSA_DEF_BLOCK:
    case SSA_DEF_FUNCTION:
      contentDef.~SSADef();
      break;
    case SSA_USE:
      contentUse.~SSAUse();
      break;
    case CHAIN:
      contentChain.~OperandChain();
      break;
    }
    kind = EMPTY;
    parent = nullptr;
  }

  template <Kind K, typename... ARGS>
  void emplace(Instr *parent, ARGS &&...args) {
    destroy();
    if constexpr (kindIsSSADef(K)) {
      new (&contentDef) SSADef(std::forward<ARGS>(args)...);
    } else if constexpr (K == SSA_USE) {
      new (&contentUse) SSAUse(std::forward<ARGS>(args)...);
    } else if constexpr (K == REG_DEF) {
      new (&contentReg) unsigned(std::forward<ARGS>(args)...);
    } else if constexpr (K == REG_USE) {
      new (&contentReg) unsigned(std::forward<ARGS>(args)...);
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
  unsigned ssaDefRegClass() {
    assert(kind == SSA_DEF_REGCLASS);
    return ssaDef().contentRegClass;
  }
  Block &ssaDefBlock() {
    assert(kind == SSA_DEF_BLOCK);
    return *ssaDef().contentBlock;
  }
  Function &ssaDefFunction() {
    assert(kind == SSA_DEF_FUNCTION);
    return *ssaDef().contentFunction;
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

  unsigned reg() const {
    assert(kind == REG_DEF || kind == REG_USE);
    return contentReg;
  }

  Instr &getParent() const {
    assert(parent);
    return *parent;
  }

  Block &getParentBlock() const;

  bool isSSADef() const { return kindIsSSADef(kind); }
  bool isSSARegDef() const { return kindIsSSARegDef(kind); }
  bool isSSAUse() const { return kindIsSSAUse(kind); }

private:
  Kind kind = EMPTY;
  Instr *parent = nullptr;
  union {
    SSADef contentDef;
    SSAUse contentUse;
    OperandChain contentChain;
    unsigned contentReg;
    int32_t contentImm32;
    Block *contentBlock;
    SSAType *contentType;
    BrCond contentBrCond;
  };
};

class Program {
public:
  Program() {}

  void addFunction(std::unique_ptr<Function> func) {
    functions.push_back(std::move(func));
  }

public:
  std::vector<std::unique_ptr<Function>> functions;
};

class Function : public IntrusiveList<Block, Function> {

public:
  Function() { funcDef.emplace<Operand::SSA_DEF_FUNCTION>(nullptr, *this); }

  Block &getEntry() {
    assert(begin() != end());
    return *begin();
  }

  Operand &getDef() { return funcDef; }

private:
  Operand funcDef;
};

class Block : public IntrusiveList<Instr, Block>,
              public IntrusiveListNode<Block, Function> {
public:
  Block() { blockDef.emplace<Operand::SSA_DEF_BLOCK>(nullptr, *this); }

  Operand &getDef() { return blockDef; }

  unsigned getNumPredecessors() { return blockDef.ssaDef().getNumUses(); }

  IntrusiveListNode<Instr, Block> &getFirstNonPhiSentry();

  void *userData = nullptr;

private:
  Operand blockDef;
};

class Instr : public IntrusiveListNode<Instr, Block> {
public:
  enum Kind {
    EMPTY,
    INSTR_START,
    CONST_INT,
    PHI,
    ADD,
    SUB,
    MUL_U,
    MUL_S,
    DIV_U,
    DIV_S,
    SL_L,
    SR_L,
    SR_A,
    AND,
    OR,
    XOR,
    BR,
    BR_COND,
    CMP,
    RET,
    CALL,
    EXT_Z,
    EXT_S,
    EXT_A,
    TRUNC,
    COPY,
    CONV,
    LOAD,
    STORE,
    ALLOCA,
    INSTR_END,
    TARGET_INSTR,
  };

  static constexpr bool kindIsInstr(unsigned kind) {
    return kind > INSTR_START && kind < INSTR_END;
  }
  static constexpr bool kindIsTarget(unsigned kind) {
    return kind > TARGET_INSTR;
  }

  static constexpr bool kindIsArtifact(unsigned kind) {
    switch (kind) {
    default:
      return false;
    case TRUNC:
    case EXT_S:
    case EXT_Z:
    case EXT_A:
      return true;
    }
  }

  static constexpr bool kindHasSideEffects(unsigned kind) {
    switch (kind) {
    default:
      return false;
    case PHI:
    case BR:
    case BR_COND:
    case RET:
    case CALL:
    case LOAD:
    case STORE:
      return true;
    }
  }

  static constexpr const char *kindName(unsigned kind) {
    switch (kind) {
    case CONST_INT:
      return "CONST_INT";
    case PHI:
      return "PHI";
    case ADD:
      return "ADD";
    case SUB:
      return "SUB";
    case MUL_U:
      return "MUL_U";
    case MUL_S:
      return "MUL_S";
    case DIV_U:
      return "DIV_U";
    case DIV_S:
      return "DIV_S";
    case SL_L:
      return "SL_L";
    case SR_L:
      return "SR_L";
    case SR_A:
      return "SR_A";
    case AND:
      return "AND";
    case OR:
      return "OR";
    case XOR:
      return "XOR";
    case BR:
      return "BR";
    case BR_COND:
      return "BR_COND";
    case CMP:
      return "CMP";
    case RET:
      return "RET";
    case CALL:
      return "CALL";
    case EXT_Z:
      return "EXT_Z";
    case EXT_S:
      return "EXT_S";
    case EXT_A:
      return "EXT_A";
    case CONV:
      return "CONV";
    case TRUNC:
      return "TRUNC";
    case COPY:
      return "COPY";
    case LOAD:
      return "LOAD";
    case STORE:
      return "STORE";
    case ALLOCA:
      return "ALLOCA";
    }
    return "UNKNOWNTARGET";
  }

  using iterator = Operand *;

  iterator begin() { return operands; }
  iterator end() { return operands + capacity; }
  iterator def_begin() { return operands; }
  iterator def_end() { return operands + numDefs; }
  iterator other_begin() { return operands + numDefs; }
  iterator other_end() { return operands + numDefs; }

  Instr(unsigned kind) : kind(kind) {}
  Instr(const Instr &o) = delete;
  Instr &operator=(const Instr &o) = delete;
  ~Instr() { deleteOperands(); }
  unsigned getKind() { return kind; }

  unsigned getNumOperands() { return numDefs + numOther; }

  unsigned getNumDefs() { return numDefs; }

  unsigned getNumOther() { return numOther; }

  Operand &getDef(unsigned n = 0) {
    assert(numDefs > n);
    return operands[n];
  }

  void deleteOperands() {
    if (!operands)
      return;
    delete[] operands;
    operands = nullptr;
    capacity = 0;
    numDefs = 0;
    numOther = 0;
  }

  void allocateOperands(unsigned cap) {
    assert(cap > 0);
    operands = new Operand[cap];
    capacity = cap;
  }

  void allocateVariadicOperands(unsigned cap) {
    assert(cap > 0);
    capacity = cap + 1;
    operands = new Operand[capacity];
    operands[cap].emplace<Operand::CHAIN>(this);
  }

  bool isVariadic() {
    assert(operands);
    return operands[capacity - 1].getKind() == Operand::CHAIN;
  }

  Operand &getOperand(unsigned n) {
    assert(n < getNumOperands());
    return operands[n];
  }

  Operand &getOperandUnchecked(unsigned n) {
    assert(n < capacity);
    return operands[n];
  }

  Operand &getLastOperand() { return getOperand(getNumOperands() - 1); }

  Operand &getChainOperand() {
    assert(isVariadic());
    return operands[capacity - 1];
  }

  template <Operand::Kind K, typename... ARGS>
  void emplaceOperand(ARGS &&...args) {
    assert(operands && getNumOperands() < capacity);
    assert(operands[getNumOperands()].kind == Operand::EMPTY);
    operands[getNumOperands()].emplace<K>(this, std::forward<ARGS>(args)...);
    if constexpr (Operand::kindIsDef(K)) {
      assert(numOther == 0 && "Defs must be inserted first");
      ++numDefs;
    } else {
      ++numOther;
    }
  }

  bool isPhi() { return kind == PHI; }
  bool isArtifact() { return kindIsTarget(kind); }
  bool isTarget() { return kindIsTarget(kind); }

private:
  unsigned kind;
  unsigned numDefs = 0, numOther = 0;
  unsigned capacity = 0;
  Operand *operands = nullptr;
};

inline SSADef::iterator &SSADef::iterator::operator++() {
  assert(mPtr);
  mPtr = mPtr->ssaUse().chNext;
  return *this;
}
