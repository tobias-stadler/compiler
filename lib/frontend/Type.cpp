#include "support/RefCount.h"
#include <cassert>
#include <frontend/Type.h>

namespace {
auto initBasicTypes(bool c) {
  std::array<BasicType, Type::NUM_BASIC> arr;
  for (int i = 0, kind = Type::BASIC_START + 1; kind < Type::BASIC_END;
       ++kind, ++i) {
    arr[i] = BasicType(static_cast<Type::Kind>(kind), Type::Qualifier(c));
    arr[i].incRefCount();
  }
  return arr;
}
std::array<BasicType, Type::NUM_BASIC> basicTypes = initBasicTypes(false);
std::array<BasicType, Type::NUM_BASIC> basicTypesC = initBasicTypes(true);
} // namespace

BasicType::BasicType(Kind kind, Qualifier qualifier) : Type(kind, qualifier) {
  assert(isBasic(kind) && "Expected basic kind");
}

CountedPtr<BasicType> BasicType::create(Kind kind, Qualifier qualifier) {
  assert(isBasic(kind) && "Expected basic kind");
  if (qualifier.onlyConst()) {
    int idx = kind - BASIC_START - 1;
    if (qualifier.isConst()) {
      return CountedPtr(&basicTypesC[idx]);
    } else {
      return CountedPtr(&basicTypes[idx]);
    }
  }
  return make_counted<BasicType>(kind, qualifier);
}

bool SymbolTable::Scope::declareSymbol(std::string name, Symbol symbol) {
  auto [_, succ] = identifierTypes.insert(
      std::make_pair(std::move(name), std::move(symbol)));
  return succ;
}

Symbol *SymbolTable::Scope::getSymbol(std::string_view name) {
  auto it = identifierTypes.find(name);
  if (it == identifierTypes.end()) {
    return nullptr;
  }
  return &it->second;
}
