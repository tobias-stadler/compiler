#pragma once

struct ArchReg {
  const char *name;
  bool noLiveness;
};

struct ArchInstr {
  const char *name;
};

class Arch {
public:
  virtual const ArchReg *getArchReg(unsigned kind) = 0;
  virtual const ArchInstr *getArchInstr(unsigned kind) = 0;
};
