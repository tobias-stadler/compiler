#pragma once

#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRPrinter.h"
#include "ir/IRVisitor.h"
#include <cassert>
#include <iostream>
#include <optional>
#include <unordered_map>

class RegTracking {
  friend class RegTrackingPass;
  friend class PrintRegTrackingPass;

public:
  using iterator = Reg;
  static const IRInfoID ID;

  Reg trackSSADef(Operand &def) {
    assert(def.isSSARegDef());
    auto [it, succ] = ssaDefToReg.try_emplace(&def, nextVReg);
    if (succ) {
      auto &reg = vRegs.emplace_back(def);
      ++nextVReg;
    }
    return it->second;
  }

  Reg getRegForSSADef(Operand &def) {
    assert(def.isSSARegDef());
    auto it = ssaDefToReg.find(&def);
    if (it == ssaDefToReg.end()) {
      return 0;
    }
    return it->second;
  }

  Reg getRegForOperand(Operand &op) {
    if (op.isReg()) {
      return op.reg();
    }
    if (op.isSSARegDef()) {
      return getRegForSSADef(op);
    }
    if (op.isSSARegUse()) {
      return getRegForSSADef(op.ssaUse().getDef());
    }
    return {};
  }

  Operand *getSSADef(Reg reg) {
    if (!isVReg(reg)) {
      return nullptr;
    }
    auto &regInfo = getVRegInfo(reg);
    return regInfo.ssaDef;
  }

  void lowerSSADef(Reg reg) {
    auto &regInfo = getVRegInfo(reg);
    assert(regInfo.ssaDef);
    auto &def = *regInfo.ssaDef;
    regInfo.ssaDef = nullptr;
    regInfo.ssaType = &def.ssaDefType();
    ssaDefToReg.erase(&def);
  }

  size_t getNumVRegs() { return vRegs.size(); }

  Reg getFirstVReg() { return firstVReg; }

  Reg createVReg(SSAType &type) {
    auto &reg = vRegs.emplace_back(type);
    return nextVReg++;
  }

  bool isVReg(Reg reg) {
    return reg >= firstVReg && (reg - firstVReg) < vRegs.size();
  }

  iterator begin() { return firstVReg; }

  iterator end() { return nextVReg; }

private:
  struct VRegInfo {
    VRegInfo(SSAType &type) : ssaType(&type) {}
    VRegInfo(Operand &ssaDef) : ssaDef(&ssaDef) {}

    Operand *ssaDef = nullptr;
    SSAType *ssaType = nullptr;
    unsigned regClass = 0;
  };

  VRegInfo &getVRegInfo(Reg reg) {
    assert(isVReg(reg));
    return vRegs[reg - firstVReg];
  }

  std::unordered_map<Operand *, Reg> ssaDefToReg;
  std::vector<VRegInfo> vRegs;
  static const Reg::num_t firstVReg = 1000;
  Reg::num_t nextVReg = firstVReg;
};

class RegTrackingPass : public IRPass<Function>,
                        protected IRVisitor<RegTrackingPass> {
  friend class IRVisitor<RegTrackingPass>;

  const char *name() override { return "RegTrackingPass"; }

  void advertise(IRInfo<Function> &info) override {
    info.advertise<RegTracking>();
  }

  void run(Function &func, IRInfo<Function> &info) override {
    data = RegTracking();
    dispatch(func);
    info.publish(data);
    info.preserveAll();
  }

  void visitOperandSSADef(Operand &op) { data.trackSSADef(op); }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<RegTracking>();
  }

  RegTracking data;
};

class PrintRegTrackingPass : public IRPass<Function> {
  const char *name() override { return "PrintRegTrackingPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    auto &printer = info.query<PrintIRVisitor>();
    auto &regTrack = info.query<RegTracking>();

    std::cout << "-- RegTracking --\n";
    for (int i = 0; i < regTrack.vRegs.size(); ++i) {
      auto &reg = regTrack.vRegs[i];
      std::cout << (i + regTrack.firstVReg) << ": ";
      if (reg.ssaDef) {
        printer.printNumberedDef(*reg.ssaDef);
      } else if (reg.ssaType) {
        printer.printSSAType(*reg.ssaType);
      } else {
        printer.printRegClass(reg.regClass);
      }
      std::cout << "\n";
    }
    std::cout << "----\n";

    info.preserveAll();
  }
};
