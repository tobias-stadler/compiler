#pragma once

#include "ir/FrameLayout.h"
#include "ir/MemoryAccess.h"
#include "ir/Operand.h"
#include "ir/RegInfo.h"
#include "support/IntrusiveList.h"
#include <cassert>
#include <cstddef>
#include <memory>
#include <unordered_map>
#include <utility>
#include <vector>

using SymbolId = unsigned;

class Function;

class GlobalDef : public ExternSSADef {
public:
  static bool is_impl(const ExternSSADef &o) {
    return kindIsGlobal(o.getKind());
  }

  enum class Linkage { EXTERNAL, INTERNAL };

  GlobalDef(Kind kind, std::string name, Linkage linkage)
      : ExternSSADef(kind), name(std::move(name)), linkage(linkage) {}

  const std::string &getName() const { return name; }

  Linkage getLinkage() { return linkage; }

private:
  std::string name;
  Linkage linkage;
};

class StaticMemory : public GlobalDef {
public:
  StaticMemory(std::string name, Linkage linkage)
      : GlobalDef(GLOBAL_STATIC_MEMORY, std::move(name), linkage) {}

  size_t size = 0;
  Alignment align{0};
  std::vector<std::byte> initializer;
};

class Program {
public:
  Program() {}

  Function *getFunction(std::string_view name);
  Function *createFunction(std::string name);

  template <typename... Args> StaticMemory &createStaticMemory(Args... args) {
    return *staticMemoryStorage.emplace_back(
        std::make_unique<StaticMemory>(args...));
  }

  auto staticMemories() {
    return staticMemoryStorage |
           std::views::transform([](auto &e) -> StaticMemory & { return *e; });
  }
  auto functions() {
    return functionStorage |
           std::views::transform([](auto &e) -> Function & { return *e; });
  }

private:
  std::unordered_map<std::string_view, Function *> functionIndex;
  std::vector<std::unique_ptr<Function>> functionStorage;
  std::vector<std::unique_ptr<StaticMemory>> staticMemoryStorage;
};

class Function : public IntrusiveList<Block, Function>, public GlobalDef {

public:
  Function(std::string name)
      : GlobalDef(GLOBAL_FUNCTION, std::move(name),
                  GlobalDef::Linkage::EXTERNAL) {}

  Block &getEntry() {
    assert(!empty());
    return getFirst();
  }

  Instr *createInstr(unsigned kind);
  Instr *createInstr(unsigned kind, unsigned cap);

  std::vector<SSAType *> paramTypes;
  std::vector<SSAType *> returnTypes;

  FrameLayout &getFrameLayout() { return frameLayout; }
  RegInfo &getRegInfo() { return regInfo; }

  template <typename... Args>
  MemoryAccessDef &createMemoryAccess(Args... args) {
    return *memoryAccess.emplace_back(
        std::make_unique<MemoryAccessDef>(args...));
  }

private:
  FrameLayout frameLayout;
  RegInfo regInfo;
  std::vector<std::unique_ptr<MemoryAccessDef>> memoryAccess;
};

class Block : public IntrusiveList<Instr, Block>,
              public IntrusiveListNode<Block, Function>,
              public ExternSSADef {
public:
  static bool is_impl(const ExternSSADef &o) {
    return o.getKind() == FUNC_BLOCK;
  }

  Block() : ExternSSADef(FUNC_BLOCK) {}

  unsigned getNumPredecessors() { return operand().ssaDef().getNumUses(); }

  IntrusiveListNode<Instr, Block> &getFirstNonPhiSentry();

  void *userData = nullptr;
};

class Instr : public IntrusiveListNode<Instr, Block> {
public:
  enum Kind {
    EMPTY,
    INSTR_START,
    UNDEFINED,
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
    REF_EXTERN,
    REF_PARAM,
    INSTR_END,
    ARCH_INSTR,
  };

  static constexpr bool kindIsInstr(unsigned kind) {
    return kind > INSTR_START && kind < INSTR_END;
  }
  static constexpr bool kindIsTarget(unsigned kind) {
    return kind > ARCH_INSTR;
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
    case REF_PARAM:
      return true;
    }
  }

  static const char *kindName(unsigned kind);

  using iterator = Operand *;

  iterator begin() { return operands; }
  iterator end() { return operands + getNumOperands(); }
  iterator def_begin() { return operands; }
  iterator def_end() { return other_begin(); }
  iterator other_begin() { return operands + numDefs; }
  iterator other_end() { return end(); }
  auto defs() { return Range{def_begin(), def_end()}; }
  auto others() { return Range{other_begin(), other_end()}; }

  Instr(unsigned kind) : kind(kind) {}
  Instr(const Instr &o) = delete;
  Instr &operator=(const Instr &o) = delete;
  ~Instr() { deleteOperands(); }
  unsigned getKind() { return kind; }

  unsigned getNumOperands() { return numDefs + numOther; }
  unsigned getNumDefs() { return numDefs; }
  unsigned getNumOthers() { return numOther; }

  Operand &getDef(unsigned n = 0) {
    assert(numDefs > n);
    return operands[n];
  }
  Operand &getOther(unsigned n = 0) {
    assert(numOther > n);
    return operands[numDefs + n];
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
    assert(!operands);
    assert(cap > 0);
    operands = new Operand[cap];
    capacity = cap;
  }

  void allocateVariadicOperands(unsigned cap) {
    assert(!operands);
    assert(cap > 0);
    capacity = cap + 1;
    operands = new Operand[capacity];
    operands[cap].emplace<Operand::OP_CHAIN>(this);
  }

  bool isVariadic() {
    assert(operands);
    return operands[capacity - 1].getKind() == Operand::OP_CHAIN;
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
  Instr &emplaceOperand(ARGS &&...args) {
    assert(operands && getNumOperands() < capacity);
    assert(operands[getNumOperands()].kind == Operand::EMPTY);
    operands[getNumOperands()].emplace<K>(this, std::forward<ARGS>(args)...);
    if constexpr (Operand::kindIsDef(K)) {
      assert(numOther == 0 && "Defs must be inserted first");
      ++numDefs;
    } else {
      ++numOther;
    }
    return *this;
  }

  Instr &addOperand(Operand &op) {
    emplaceOperand<Operand::EMPTY>();
    getLastOperand() = op;
    return *this;
  }

  bool isPhi() { return kind == PHI; }
  bool isArtifact() { return kindIsArtifact(kind); }
  bool isTarget() { return kindIsTarget(kind); }
  bool isCopy() { return kind == COPY; }

private:
  unsigned kind;
  unsigned numDefs = 0, numOther = 0;
  unsigned capacity = 0;
  Operand *operands = nullptr;
};

inline Instr *Function::createInstr(unsigned kind) { return new Instr(kind); }
inline Instr *Function::createInstr(unsigned kind, unsigned cap) {
  Instr *i = createInstr(kind);
  i->allocateOperands(cap);
  return i;
}
