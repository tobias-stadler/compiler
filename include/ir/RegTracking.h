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

  void lowerSSADef(Reg reg) {
    auto &regInfo = getVRegInfo(reg);
    assert(regInfo.kind == TRACKING_SSA_DEF);
    auto &def = *regInfo.contentSSADef;
    switch (def.getKind()) {
    case Operand::SSA_DEF_TYPE:
      regInfo.kind = TYPE;
      regInfo.contentType = &def.ssaDefType();
      break;
    case Operand::SSA_DEF_REGCLASS:
      regInfo.kind = REGCLASS;
      regInfo.contentRegClass = def.ssaDefRegClass();
      break;
    default:
      break;
    }
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

private:
  enum VRegKind {
    TRACKING_SSA_DEF,
    REGCLASS,
    TYPE,
  };
  struct VRegInfo {
    VRegInfo(SSAType &type) : kind(TYPE), contentType(&type) {}
    VRegInfo(Operand &ssaDef)
        : kind(TRACKING_SSA_DEF), contentSSADef(&ssaDef) {}

    VRegKind kind;
    union {
      Operand *contentSSADef;
      SSAType *contentType;
      unsigned contentRegClass;
    };
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
  const char *name() override { return "RegTrackingPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    auto &printer = info.query<PrintIRVisitor>();
    auto &regTrack = info.query<RegTracking>();

    std::cout << "-- RegTracking --\n";
    for (int i = 0; i < regTrack.vRegs.size(); ++i) {
      auto &reg = regTrack.vRegs[i];
      std::cout << (i + regTrack.firstVReg) << ": ";
      switch (reg.kind) {
      case RegTracking::TRACKING_SSA_DEF:
        printer.printNumberedDef(*reg.contentSSADef);
        break;
      case RegTracking::REGCLASS:
        printer.printRegClass(reg.contentRegClass);
        break;
      case RegTracking::TYPE:
        printer.printSSAType(*reg.contentType);
        break;
      }
      std::cout << "\n";
    }
    std::cout << "----\n";

    info.preserveAll();
  }
};
