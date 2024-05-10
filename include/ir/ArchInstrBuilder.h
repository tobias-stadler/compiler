#pragma once

#include "ir/InstrBuilder.h"

class ArchInstrBuilder {
public:
  virtual void frameStoreReg(InstrBuilder &b, Reg reg,
                             FrameDef &frameDef) const = 0;
  virtual void frameLoadReg(InstrBuilder &b, Reg reg,
                            FrameDef &frameDef) const = 0;
};
