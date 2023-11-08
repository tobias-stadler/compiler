#pragma once

class Arch {
public:
  virtual const char *getInstrKindName(unsigned kind) = 0;
  virtual const char *getRegisterKindName(unsigned kind) = 0;
};
