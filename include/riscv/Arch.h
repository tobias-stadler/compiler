#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRPatExecutor.h"
#include "ir/IRVisitor.h"
#include "ir/InstrBuilder.h"
#include <array>
#include <cassert>
#include <initializer_list>

namespace riscv {

#include "riscv/Arch.dsl.riscv.h"

class Arch : public ::Arch {
public:
  std::span<const ArchReg> getArchRegs() override { return archRegs; };
  const ArchReg *getArchReg(unsigned kind) override {
    return riscv::getArchReg(kind);
  }
  const ArchInstr *getArchInstr(unsigned kind) override {
    return riscv::getArchInstr(kind);
  }
  const ArchRegClass *getArchRegClass(unsigned kind) override {
    return riscv::getArchRegClass(kind);
  }
};

class ABILoweringPass : public IRPass<Function> {
  const char *name() override { return "RiscVABILoweringPass"; }

  static constexpr auto inRegs =
      std::to_array<Reg>({X10, X11, X12, X13, X14, X15, X16, X17});
  static constexpr auto outRegs = std::to_array<Reg>({X10, X11});
  static constexpr Reg raReg = X1;

  void run(Function &func, IRInfo<Function> &info) override {
    if (func.empty()) {
      return;
    }
    std::vector<Instr *> worklist;

    auto &abiTy = IntSSAType::get(32);

    auto &entryBB = func.getFirst();
    InstrBuilder ir(entryBB.getFirstSentry());
    size_t paramNum = 0;
    for (auto it = entryBB.begin(), itEnd = entryBB.end(); it != itEnd;) {
      auto &instr = *it;
      ++it;
      if (instr.getKind() != Instr::REF_PARAM) {
        continue;
      }
      worklist.push_back(&instr);
      if (paramNum < inRegs.size()) {
        ir.emitCopy(abiTy, inRegs[paramNum]);
        if (instr.getDef().ssaDefType() != abiTy) {
          assert(false && "Unsupported parameter type");
        }
        instr.getDef().ssaDef().replaceAllUses(ir.getDef());
      } else {
        assert(false && "Stack passing unimplemented");
      }
      ++paramNum;
    }
    for (auto *instr : worklist) {
      instr->deleteThis();
    }
    worklist.clear();

    for (auto &block : func) {
      for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
        auto &instr = *it;
        ++it;
        InstrBuilder ir(instr);
        if (instr.getKind() == Instr::RET) {
          size_t paramNum = 0;
          for (auto &op : instr) {
            assert(op.ssaUse().getDef().ssaDefType() == abiTy);
            assert(paramNum < outRegs.size());
            ir.emitCopy(outRegs[paramNum], op.ssaUse().getDef());
            ++paramNum;
          }
          Instr *i = func.createInstr(JALR, 3);
          i->emplaceOperand<Operand::REG_DEF>(X0);
          i->emplaceOperand<Operand::REG_USE>(raReg);
          i->emplaceOperand<Operand::IMM32>(0);
          ir.emit(i);
          instr.deleteThis();
        } else if (instr.getKind() == Instr::CALL) {
          size_t paramNum = 0;
          Operand &funcDef = instr.getOther().ssaUse().getDef();
          for (auto it = instr.other_begin() + 1, itEnd = instr.other_end();
               it != itEnd; ++it) {
            auto &op = *it;
            assert(op.ssaUse().getDef().ssaDefType() == abiTy);
            assert(paramNum < inRegs.size());
            ir.emitCopy(inRegs[paramNum], op.ssaUse().getDef());
            ++paramNum;
          }
          Instr *i = func.createInstr(PSEUDO_CALL, 1);
          i->emplaceOperand<Operand::SSA_USE>(funcDef);
          ir.emit(i);
          paramNum = 0;
          for (auto it = instr.def_begin(), itEnd = instr.def_end();
               it != itEnd; ++it) {
            auto &op = *it;
            assert(op.ssaDefType() == abiTy);
            assert(paramNum < outRegs.size());
            ir.emitCopy(abiTy, outRegs[paramNum]);
            op.ssaDef().replaceAllUses(ir.getDef());
            ++paramNum;
          }
          instr.deleteThis();
        }
      }
    }
  }
};

class InstrSelect : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class PreISelExpansion : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class PreISelCombine : public IRPatExecutor {
  bool execute(Instr &instr) override;
};

class PostRALowering : public IRPass<Function>,
                       public IRVisitor<PostRALowering> {
public:
  void run(Function &func, IRInfo<Function> &info) override { dispatch(func); }

  void visitInstr(Instr &instr) {
    switch (instr.getKind()) {
    case BEQ:
    case BNE:
    case BLT:
    case BLTU:
    case BGE:
    case BGEU:
      lowerCondBr(instr);
      break;
    case Instr::COPY:
      lowerCopy(instr);
      break;
    }
  }

  void lowerCopy(Instr &i) {
    if (i.getOperand(0).reg() == i.getOperand(1).reg()) {
      i.deleteThis();
      return;
    }
    Instr *newI = new Instr(ADDI);
    newI->allocateOperands(3);
    newI->emplaceOperand<Operand::REG_DEF>(i.getOperand(0).reg());
    newI->emplaceOperand<Operand::REG_USE>(i.getOperand(1).reg());
    newI->emplaceOperand<Operand::IMM32>(0);
    i.insertPrev(newI);
    i.deleteThis();
  }

  void lowerCondBr(Instr &i) {
    Instr *newBr = new Instr(i.getKind());
    newBr->allocateOperands(3);
    newBr->addOperand(i.getOperand(0));
    newBr->addOperand(i.getOperand(1));
    newBr->emplaceOperand<Operand::SSA_USE>(i.getOperand(2).ssaUse().getDef());
    Instr *newJmp = new Instr(JAL);
    newJmp->allocateOperands(2);
    newJmp->emplaceOperand<Operand::REG_USE>(X0);
    newJmp->addOperand(i.getOperand(3));
    i.insertPrev(newBr);
    i.insertPrev(newJmp);
    i.deleteThis();
  }

  const char *name() override { return "RiscVPostRALoweringPass"; }
};

class AsmPrinterPass : public IRPass<Program> {
public:
  AsmPrinterPass(std::ostream &out) : out(out) {}
  const char *name() override { return "RiscVAsmPrinterPass"; }
  void run(Program &prog, IRInfo<Program> &info) override {
    out << ".option nopic\n";
    out << ".attribute arch, \"rv32i\"\n";
    out << ".text\n";
    for (auto &func : prog.functions) {
      printFunction(*func);
    }
  }

  void printFunction(Function &func) {
    blockToNum.clear();
    unsigned blockNum = 0;
    for (auto &block : func) {
      blockToNum[&block] = blockNum++;
    }
    printFunctionLabel(func);
    out << ":\n";
    for (auto &block : func) {
      printBlock(block);
    }
  }

  void printBlock(Block &block) {
    printBlockLabel(block);
    out << ":\n";
    currIndent = 1;
    for (auto &instr : block) {
      printIndent();
      printInstr(instr);
      out << "\n";
    }
  }

  void printInstr(Instr &instr) {
    const ArchInstr *archInstr = getArchInstr(instr.getKind());
    if (!archInstr || !archInstr->alias) {
      assert(false && "Illegal instruction");
      return;
    }
    out << archInstr->alias;
    if (instr.getNumOperands() != 0) {
      out << " ";
    }
    bool first = true;
    for (auto &op : instr) {
      if (op.isImplicit())
        continue;
      if (first) {
        first = false;
      } else {
        out << ", ";
      }
      printOperand(op);
    }
  }

  void printFunctionLabel(Function &func) { out << func.getName(); }

  void printBlockLabel(Block &block) { out << ".Lbb" << blockToNum[&block]; }

  void printOperand(Operand &op) {
    switch (op.getKind()) {
    case Operand::EMPTY:
      break;
    case Operand::SSA_DEF_TYPE:
    case Operand::SSA_DEF_BLOCK:
    case Operand::SSA_DEF_GLOBAL:
    case Operand::TYPE:
    case Operand::BLOCK:
    case Operand::BRCOND:
    case Operand::CHAIN:
      assert(false && "Illegal operand");
      break;
    case Operand::SSA_USE: {
      Operand &def = op.ssaUse().getDef();
      if (def.getKind() == Operand::SSA_DEF_BLOCK) {
        printBlockLabel(op.ssaUse().getDef().ssaDefBlock());
        break;
      }
      if (def.getKind() == Operand::SSA_DEF_GLOBAL) {
        out << op.ssaDefGlobal().getName();
        break;
      }
      assert(false && "Illegal operand");
      break;
    }
    case Operand::REG_DEF:
    case Operand::REG_USE: {
      const ArchReg *archReg = getArchReg(op.reg());
      if (!archReg || !archReg->alias) {
        assert(false && "Illegal register");
        return;
      }
      out << archReg->alias;
      break;
    }
    case Operand::IMM32: {
      out << op.imm32();
      break;
    }
    }
  }

  void printIndent() {
    for (unsigned i = 0; i < currIndent; ++i) {
      out << "  ";
    }
  }

  unsigned currIndent;
  std::unordered_map<Block *, unsigned> blockToNum;
  std::ostream &out;
};

} // namespace riscv
