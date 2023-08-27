#pragma once

#include "support/IntrusiveList.h"
#include <cassert>
#include <cstdint>
#include <new>
#include <type_traits>
#include <utility>

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

  Kind getKind() { return kind; }

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
  Operand &getDef() {
    assert(def);
    return *def;
  }
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
  SSADef(unsigned regClass) : contentRegClass(regClass) {}
  SSADef(Block &block) : contentBlock(&block) {}
  SSADef(Function &func) : contentFunction(&func) {}

  SSADef(const SSADef &o) = delete;
  SSADef &operator=(const SSADef &o) = delete;
  ~SSADef() { unlinkAllUses(); }

  void unlinkAllUses();

private:
  Operand *chNext = nullptr;
  union {
    SSAType *contentType;
    unsigned contentRegClass;
    Block *contentBlock;
    Function *contentFunction;
  };
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
    DEF_REG,
    USE_REG,
    BLOCK,
    IMM32,
    BRCOND,
    CHAIN,
  };
  Operand() {}
  Operand(const Operand &o) = delete;
  Operand &operator=(const Operand &o) = delete;
  ~Operand() { destroy(); }

  static constexpr bool kindIsSSA(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
    case SSA_DEF_BLOCK:
    case SSA_DEF_FUNCTION:
    case SSA_USE:
      return true;
    default:
      return false;
    }
  }

  static constexpr bool kindIsDef(Kind kind) {
    switch (kind) {
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
    case SSA_DEF_BLOCK:
    case SSA_DEF_FUNCTION:
    case DEF_REG:
      return true;
    default:
      return false;
    }
  }

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
      delete[] contentChNext;
      break;
    }
    kind = EMPTY;
    parent = nullptr;
  }

  template <Kind K, typename... ARGS>
  void emplace(Instr *parent, ARGS &&...args) {
    destroy();
    if constexpr (kindIsDef(K) && kindIsSSA(K)) {
      new (&contentDef) SSADef(std::forward<ARGS>(args)...);
    } else if constexpr (K == SSA_USE) {
      new (&contentUse) SSAUse(std::forward<ARGS>(args)...);
    } else if constexpr (K == IMM32) {
      new (&contentImm32) int32_t(std::forward<ARGS>(args)...);
    } else if constexpr (K == BRCOND) {
      new (&contentBrCond) BrCond(std::forward<ARGS>(args)...);
    } else if constexpr (K == BLOCK) {
      new (&contentChNext) Block *(std::forward<ARGS>(args)...);
    } else if constexpr (K == CHAIN) {
      new (&contentChNext) Operand *(std::forward<ARGS>(args)...);
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
    assert(kindIsSSA(kind) && kindIsDef(kind));
    return contentDef;
  }
  SSAType &ssaDefType() {
    assert(kind == SSA_DEF_TYPE);
    return *ssaDef().contentType;
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

  Block &useBlock() {
    assert(kind == BLOCK && contentBlock);
    return *contentBlock;
  }

  BrCond brCond() {
    assert(kind == BRCOND);
    return contentBrCond;
  }

  Operand *chain() {
    assert(kind == CHAIN);
    return contentChNext;
  }

  void ssaDefAddUse(Operand &useOp) {
    SSADef &def = ssaDef();
    SSAUse &use = useOp.ssaUse();
    use.chNext = def.chNext;
    use.chPrev = nullptr;
    use.def = this;

    if (def.chNext) {
      def.chNext->ssaUse().chPrev = &useOp;
    }

    def.chNext = &useOp;
  }

  SSAUse &ssaUse() {
    assert(kind == SSA_USE);
    return contentUse;
  }

  int32_t imm32() {
    assert(kind == IMM32);
    return contentImm32;
  }

  Instr &getParent() {
    assert(parent);
    return *parent;
  }

private:
  Kind kind = EMPTY;
  Instr *parent = nullptr;
  union {
    SSADef contentDef;
    SSAUse contentUse;
    unsigned contentReg;
    int32_t contentImm32;
    Operand *contentChNext;
    Block *contentBlock;
    BrCond contentBrCond;
  };
};

class Function : public IntrusiveList<Block, Function> {

public:
  Function() { funcDef.emplace<Operand::SSA_DEF_FUNCTION>(nullptr, *this); }

private:
  Operand funcDef;
};

class Block : public IntrusiveList<Instr, Block>,
              public IntrusiveListNode<Block, Function> {
public:
  Block() { blockDef.emplace<Operand::SSA_DEF_BLOCK>(nullptr, *this); }

  Operand &getDef() { return blockDef; }

  void *userData = nullptr;

private:
  Operand blockDef;
};

class Instr : public IntrusiveListNode<Instr, Block> {
public:
  enum Kind {
    CONST_INT,
    INSTR_START,
    INSTR_PHI,
    INSTR_ADD,
    INSTR_SUB,
    INSTR_MULU,
    INSTR_MULS,
    INSTR_DIV,
    INSTR_SHL,
    INSTR_SHR,
    INSTR_SHR_A,
    INSTR_AND,
    INSTR_OR,
    INSTR_XOR,
    INSTR_BR,
    INSTR_BR_COND,
    INSTR_CMP,
    INSTR_RET,
    INSTR_CALL,
    INSTR_EXTZ,
    INSTR_EXTS,
    INSTR_TRUNC,
    INSTR_LOAD,
    INSTR_STORE,
    INSTR_ALLOCA,
    INSTR_END,
    TARGET_INSTR,
  };

  static constexpr bool kindIsInstr(unsigned kind) {
    return kind > INSTR_START && kind < INSTR_END;
  }
  static constexpr bool kindIsTarget(unsigned kind) {
    return kind > TARGET_INSTR;
  }

  static constexpr const char *kindName(unsigned kind) {
    switch (kind) {
    case CONST_INT:
      return "CONSTint";
    case INSTR_PHI:
      return "PHI";
    case INSTR_ADD:
      return "ADD";
    case INSTR_SUB:
      return "SUB";
    case INSTR_MULU:
      return "MULu";
    case INSTR_MULS:
      return "MULs";
    case INSTR_DIV:
      return "DIV";
    case INSTR_SHL:
      return "SHL";
    case INSTR_SHR:
      return "SHR";
    case INSTR_SHR_A:
      return "SHRA";
    case INSTR_AND:
      return "AND";
    case INSTR_OR:
      return "OR";
    case INSTR_XOR:
      return "XOR";
    case INSTR_BR:
      return "BR";
    case INSTR_BR_COND:
      return "BRcond";
    case INSTR_CMP:
      return "CMP";
    case INSTR_RET:
      return "RET";
    case INSTR_CALL:
      return "CALL";
    case INSTR_EXTZ:
      return "EXTz";
    case INSTR_EXTS:
      return "EXTs";
    case INSTR_TRUNC:
      return "TRUNC";
    case INSTR_LOAD:
      return "LOAD";
    case INSTR_STORE:
      return "STORE";
    case INSTR_ALLOCA:
      return "ALLOCA";
    }
    if (kindIsTarget(kind)) {
      return "T";
    }
    return "???";
  }

  using iterator = Operand *;

  iterator begin() { return operands; }
  iterator end() { return operands + getNumOperands(); }

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

  Operand &getOperand(unsigned n) {
    assert(n < getNumOperands());
    return operands[n];
  }

  void deleteOperands() {
    if (!operands)
      return;
    delete[] operands;
    operands = nullptr;
    numDefs = 0;
    numOther = 0;
    capacity = 0;
  }

  void allocateOperands(unsigned cap) {
    assert(cap > 0);
    deleteOperands();
    operands = new Operand[cap];
    capacity = cap;
  }

  bool isVariadic() {
    assert(operands);
    return operands[capacity - 1].getKind() == Operand::CHAIN;
  }

  void allocateVariadicOperands(unsigned cap) {
    assert(cap > 0);
    deleteOperands();
    capacity = cap + 1;
    operands = new Operand[capacity];
    operands[cap].emplace<Operand::CHAIN>(nullptr, nullptr);
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

private:
  unsigned kind;
  unsigned numDefs = 0, numOther = 0;
  unsigned capacity = 0;
  Operand *operands = nullptr;
};

class InstrPtr {
public:
  InstrPtr(Instr *instr) : instr(instr) { assert(instr); }

  Instr &operator*() { return *instr; }
  Instr *operator->() { return instr; }

protected:
  Instr *instr;
};

class PhiInstrPtr : public InstrPtr {
public:
  PhiInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr->getKind() == Instr::INSTR_PHI);
  }

  void setup(SSAType &type, unsigned numPred) {
    instr->allocateOperands(1 + 2 * numPred);
    instr->emplaceOperand<Operand::SSA_DEF_TYPE>(type);
  }

  void addPred(Operand &def, Block &pred) {
    instr->emplaceOperand<Operand::SSA_USE>(def);
    instr->emplaceOperand<Operand::BLOCK>(&pred);
  }
};
