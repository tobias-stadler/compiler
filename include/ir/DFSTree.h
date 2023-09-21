#pragma once

#include "ir/IR.h"
#include <unordered_map>
#include <vector>

class DFSTree {
public:
  unsigned numBlocks = 0;
  std::unordered_map<Block *, int> preorderNum;
  std::vector<Block *> preorderBlock;
  std::vector<int> preorderParent;
  std::vector<Block*> postorderBlock;
};
