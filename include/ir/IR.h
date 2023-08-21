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
  IntSSAType(unsigned bits) : SSAType(INT), bits(bits) {}

  static IntSSAType &get(unsigned bits);

  unsigned getBits() { return bits; }

private:
  unsigned bits;
};

class Instr;
class Block;
class Function;

class SSAUse;
class SSADef;

class SSAUse {
  friend class SSADef;

public:
  SSAUse(SSADef &def);
  ~SSAUse() { unlink(); }
  void unlink();

private:
  SSADef *def = nullptr;
  SSAUse *chNext = nullptr;
  SSAUse *chPrev = nullptr;
};

class SSADef {
  friend class SSAUse;

public:
  void addUse(SSAUse &use);
  SSADef(SSAType &type) : type(&type) {}
  SSADef(unsigned regClass) : regClass(regClass) {}

private:
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
  };
  Operand() {}
  ~Operand() { destroy(); }

  Kind getKind() { return kind; }

  void destroy() {
    switch (kind) {
    default:
      return;
    case SSA_DEF_TYPE:
    case SSA_DEF_REGCLASS:
      def.~SSADef();
      break;
    case SSA_USE:
      use.~SSAUse();
      break;
    }
    kind = EMPTY;
  }

  template <Kind K, typename... ARGS>
  void emplace(Instr &parent, ARGS &&...args) {
    destroy();
    if constexpr (K == SSA_DEF_TYPE || K == SSA_DEF_REGCLASS) {
      new (&def) SSADef(std::forward<ARGS>(args)...);
    } else if constexpr (K == SSA_USE) {
      new (&use) SSAUse(std::forward<ARGS>(args)...);
    }
    this->parent = &parent;
    kind = K;
  }

  SSADef &ssaDef() {
    assert(kind == SSA_DEF_TYPE || kind == SSA_DEF_REGCLASS);
    return def;
  }

  SSAUse &ssaUse() {
    assert(kind == SSA_USE);
    return use;
  }

private:
  Kind kind = EMPTY;
  Instr *parent;
  union {
    SSADef def;
    SSAUse use;
    unsigned reg;
    int32_t imm32;
    Block *block;
    Function *function;
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
    INSTR_BR,
    INSTR_CMP,
    INSTR_RET,
    INSTR_LOAD,
    INSTR_STORE,
    INSTR_ALLOCA,
    INSTR_END,
    TARGET_INSTR,
  };
  Instr(unsigned kind) : kind(kind) {}
  unsigned getKind() { return kind; }

  operator Operand &() {
    assert(numDefs > 0);
    return operands[0];
  }

  void setOperands(unsigned numDefs, unsigned numUses, Operand *operands) {
    this->numDefs = numDefs;
    this->numUses = numUses;
    this->operands = operands;
  }

private:
  unsigned kind;
  unsigned numUses = 0, numDefs = 0;
  Operand *operands = nullptr;
};

template <class T> class InstrRef {
public:
  InstrRef(Instr *instr) : instr(instr) {
    assert(instr && "Instr cannot be nullptr");
  }

  operator T &() { return *instr; }

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

  Instr &emitConstInt(IntSSAType &type, int32_t val) {
    Instr *i = new Instr(Instr::CONST_INT);
    Operand *o = new Operand[2];
    o[0].emplace<Operand::SSA_DEF_TYPE>(*i, type);
    o[1].emplace<Operand::IMM32>(*i, val);
    i->setOperands(1, 1, o);
    emit(i);
    return *i;
  }

  Instr &emitBinop(Instr::Kind kind, SSAType &type, Operand &lhs,
                   Operand &rhs) {
    Instr *i = new Instr(kind);
    Operand *o = new Operand[3];
    o[0].emplace<Operand::SSA_DEF_TYPE>(*i, type);
    o[1].emplace<Operand::SSA_USE>(*i, lhs.ssaDef());
    o[2].emplace<Operand::SSA_USE>(*i, rhs.ssaDef());
    i->setOperands(1, 2, o);
    emit(i);
    return *i;
  }

  Instr &emitAdd(SSAType &type, Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::INSTR_ADD, type, lhs, rhs);
  }

private:
  IntrusiveListNode<Instr, Block> *insertionPoint = nullptr;
};
