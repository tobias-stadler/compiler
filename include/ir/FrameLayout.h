#pragma once

#include "ir/IR.h"
#include <vector>

class FrameEntry {
public:
  SSAType &elementType;
  unsigned numElements;
};

class FrameLayout {
public:
  std::vector<FrameLayout> entries;
};
