#pragma once

#include "ir/IR.h"
#include <span>

struct ArchReg {
  const Reg reg;
  const char *name;
  const bool noLiveness;
};

struct ArchInstr {
  const unsigned kind;
  const char *name;
};

struct ArchRegClass {
  const unsigned kind;
  const char *name;
  const std::span<const ArchReg*> regs;
};

class Arch {
public:
  virtual const ArchReg *getArchReg(unsigned kind) = 0;
  virtual const ArchInstr *getArchInstr(unsigned kind) = 0;
  virtual const ArchRegClass *getArchRegClass(unsigned kind) = 0;
};
