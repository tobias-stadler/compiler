#pragma once
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRVisitor.h"
#include <optional>
#include <unordered_map>

class NumberingIRVisitor : public IRVisitor<NumberingIRVisitor> {
  friend class IRVisitor;

public:
  static const IRInfoID ID;

  unsigned defNum(Operand &op);
  std::optional<unsigned> getNum(Operand &op);

protected:
  void visitFunction(Function &func);
  void visitOperandSSADef(Operand &op);

private:
  std::unordered_map<Operand *, unsigned> ssaDefs;
  unsigned nextSSADefNum = 0;
};

class PrintIRVisitor : public IRVisitor<PrintIRVisitor> {
  friend class IRVisitor;

public:
  static const IRInfoID ID;

  void printNumberedDef(Operand &op);
  void printSSAType(SSAType &type);

  NumberingIRVisitor &getNumbering() { return numbering; }

protected:
  void visitProgram(Program &prog);
  void visitFunction(Function &func);
  void visitBlock(Block &block);
  void visitInstr(Instr &instr);
  void visitOperand(Operand &op);
  NumberingIRVisitor numbering;
};

class PrintIRPass : public IRPass<Function> {
  const char *name() { return "PrintIRPass"; }

  void advertise(IRInfo<Function> &info) { info.advertise<PrintIRVisitor>(); }

  void invalidate(IRInfo<Function> &) {}

  void run(Function &obj, IRInfo<Function> &info) {
    irVisitor = PrintIRVisitor();
    irVisitor.dispatch(obj);
    info.publish(irVisitor);
  }

  PrintIRVisitor irVisitor;
};

template <class T> void PrintIR(T &ir);
