#include "frontend/Semantics.h"
#include <algorithm>

Symbol *Scope::declareSymbol(IdentId identId, Symbol symbol) {
  auto [it, succ] = symbols.try_emplace(identId, std::move(symbol));
  if (succ) {
    return &it->second;
  }
  return nullptr;
}

Symbol *Scope::getSymbol(IdentId identId) {
  auto it = symbols.find(identId);
  if (it == symbols.end()) {
    return nullptr;
  }
  return &it->second;
}
