#pragma once

#include "support/IntrusiveList.h"
#include <cassert>
#include <cstdint>
#include <new>
#include <utility>

class SSAType {
public:
  enum Kind {
    VOID,
    INT,
    PTR,
  };
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

class SSAUse;
class SSADef;

class SSAUse {
  friend class SSADef;

public:
  SSAUse(Instr &parent, SSADef &def);
  ~SSAUse() { unlink(); }
  SSADef &getDef() {
    assert(def);
    return *def;
  }
  void unlink();

private:
  Instr *parent = nullptr;
  SSADef *def = nullptr;
  SSAUse *chNext = nullptr;
  SSAUse *chPrev = nullptr;
};

class SSADef {
  friend class SSAUse;

public:
  void addUse(SSAUse &use);
  SSADef(Instr &parent, SSAType &type) : parent(&parent), type(&type) {}
  SSADef(Instr &parent, unsigned regClass)
      : parent(&parent), regClass(regClass) {}

  ~SSADef() { unlinkAllUses(); }

  Instr &getParent() {
    assert(parent);
    return *parent;
  }

  void unlinkAllUses();

private:
  Instr *parent = nullptr;
  SSAUse *chNext = nullptr;
  union {
    SSAType *type;
    unsigned regClass;
  };
};

class Operand {
  friend class Instr;

public:
  enum Kind {
    EMPTY,
    SSA_DEF_TYPE,
    SSA_DEF_REGCLASS,
    SSA_USE,
    REG_DEF,
    REG_USE,
    IMM32,
    BLOCK,
    FUNCTION,
    CHAIN,
  };
  Operand() {}
  ~Operand() { destroy(); }

  Kind getKind() { return kind; }

  void destroy() {
    switch (kind) {
    default:
      break;
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
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
  }

  template <Kind K, typename... ARGS> void emplace(ARGS &&...args) {
    destroy();
    if constexpr (K == SSA_DEF_TYPE || K == SSA_DEF_REGCLASS) {
      new (&contentDef) SSADef(std::forward<ARGS>(args)...);
    } else if constexpr (K == SSA_USE) {
      new (&contentUse) SSAUse(std::forward<ARGS>(args)...);
    } else if constexpr (K == IMM32) {
      new (&contentImm32) int32_t(std::forward<ARGS>(args)...);
    }
    kind = K;
  }

  SSADef &ssaDef() {
    assert(kind == SSA_DEF_TYPE || kind == SSA_DEF_REGCLASS);
    return contentDef;
  }

  SSAUse &ssaUse() {
    assert(kind == SSA_USE);
    return contentUse;
  }

  int32_t imm32() {
    assert(kind == IMM32);
    return contentImm32;
  }

private:
  Kind kind = EMPTY;
  union {
    SSADef contentDef;
    SSAUse contentUse;
    unsigned contentReg;
    int32_t contentImm32;
    Block *contentBlock;
    Function *contentFunction;
    Operand *contentChNext;
  };
};

class Instr;
class Block;
class Function;

class Function : public IntrusiveList<Block, Function> {};

class Block : public IntrusiveList<Instr, Block>,
              public IntrusiveListNode<Block, Function> {};

class Instr : public IntrusiveListNode<Instr, Block> {
public:
  enum Kind {
    CONST_INT,
    INSTR_START,
    INSTR_PHI,
    INSTR_ADD,
    INSTR_SUB,
    INSTR_MUL,
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
    INSTR_ZEXT,
    INSTR_SEXT,
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
      return "CONST_INT";
    case INSTR_PHI:
      return "PHI";
    case INSTR_ADD:
      return "ADD";
    default:
      break;
    }
    return "???";
  }

  using iterator = Operand *;

  iterator begin() {
    assert(operands && "Cannot use iterator without operands");
    return operands;
  }
  iterator end() { return operands + getNumOperads(); }

  Instr(unsigned kind) : kind(kind) {}
  Instr(const Instr &o) = delete;
  Instr &operator=(const Instr &o) = delete;
  ~Instr() { deleteOperands(); }
  unsigned getKind() { return kind; }

  void setOperands(unsigned numDefs, unsigned numOther, Operand *operands) {
    this->numDefs = numDefs;
    this->numOther = numOther;
    this->operands = operands;
  }

  unsigned getNumOperads() { return numDefs + numOther; }

  unsigned getNumDefs() { return numDefs; }

  unsigned getNumOther() { return numOther; }

  Operand &getDef(unsigned n = 0) {
    assert(numDefs > n);
    return operands[n];
  }

  Operand &getOperand(unsigned n) {
    assert(n < getNumOperads());
    return operands[n];
  }

  void deleteOperands() {
    delete[] operands;
    operands = nullptr;
    numDefs = 0;
    numOther = 0;
  }

private:
  unsigned kind;
  unsigned numOther = 0, numDefs = 0;
  Operand *operands = nullptr;
};

template <class T> class InstrRef {
public:
  InstrRef(Instr *instr) : instr(instr) {
    assert(instr && "Instr cannot be nullptr");
  }

private:
  Instr *instr;
};

class AddInstrRef : public InstrRef<AddInstrRef> {
  AddInstrRef(Instr *instr) : InstrRef(instr) {
    assert(instr->getKind() == Instr::INSTR_ADD);
  }
};

class InstrEmitter {
public:
  InstrEmitter() = default;
  InstrEmitter(Instr &insertionPoint) : insertionPoint(&insertionPoint) {}
  InstrEmitter(Block &insertionBlock)
      : insertionPoint(&insertionBlock.getSentryEnd()) {}

  void emit(Instr *instr) {
    if (insertionPoint) {
      insertionPoint->insertPrev(instr);
    }
  }

  Instr *emitConstInt(IntSSAType &type, int32_t val) {
    Instr *i = new Instr(Instr::CONST_INT);
    Operand *o = new Operand[2];
    o[0].emplace<Operand::SSA_DEF_TYPE>(*i, type);
    o[1].emplace<Operand::IMM32>(val);
    i->setOperands(1, 1, o);
    emit(i);
    return i;
  }

  Instr *emitBinop(Instr::Kind kind, SSAType &type, Operand &lhs,
                   Operand &rhs) {
    Instr *i = new Instr(kind);
    Operand *o = new Operand[3];
    o[0].emplace<Operand::SSA_DEF_TYPE>(*i, type);
    o[1].emplace<Operand::SSA_USE>(*i, lhs.ssaDef());
    o[2].emplace<Operand::SSA_USE>(*i, rhs.ssaDef());
    i->setOperands(1, 2, o);
    emit(i);
    return i;
  }

  Instr *emitAdd(SSAType &type, Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_ADD, type, lhs, rhs);
  }
  Instr *emitSub(SSAType &type, Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_SUB, type, lhs, rhs);
  }
  Instr *emitMul(SSAType &type, Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_MUL, type, lhs, rhs);
  }

private:
  IntrusiveListNode<Instr, Block> *insertionPoint = nullptr;
};
