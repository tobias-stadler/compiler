#pragma once

class Arch {
public:
  virtual const char *getInstrKindName(unsigned kind) = 0;
};
