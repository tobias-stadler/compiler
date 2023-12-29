#pragma once

#include "c/Type.h"
#include "ir/IR.h"
#include "support/RefCount.h"
#include <unordered_map>

namespace c {

class Symbol {
  friend class SymbolTable;

public:
  enum Kind { EMPTY, TYPEDEF, EXTERN, STATIC, AUTO, REGISTER };
  enum class Linkage { EXTERNAL, INTERNAL, NONE };
  enum class StorageDuration { STATIC, THREAD, AUTOMATIC, ALLOCATED, NONE };
  Symbol(Kind kind, CountedPtr<Type> type, std::string_view name)
      : kind(kind), type(std::move(type)), name(name) {}

  Kind getKind() { return kind; }

  CountedPtr<Type> &getType() { return type; }

  SSASymbolId getId() { return id; }

  bool isAddrTaken() { return addrTaken; }
  void setAddrTaken(bool taken) { addrTaken = taken; }

  Linkage getLinkage() {
    switch (kind) {
    case EXTERN:
      return Linkage::EXTERNAL;
    case STATIC:
      return Linkage::INTERNAL;
    default:
      return Linkage::NONE;
    }
  }

  StorageDuration getStorageDuration() {
    switch (kind) {
    case EXTERN:
    case STATIC:
      return StorageDuration::STATIC;
    case AUTO:
    case REGISTER:
      return StorageDuration::AUTOMATIC;
    case EMPTY:
    case TYPEDEF:
      return StorageDuration::NONE;
    }
  }

  std::string_view getName() const { return name; }

private:
  Kind kind;
  CountedPtr<Type> type;
  SSASymbolId id = 0;
  std::string_view name;
  bool addrTaken = false;
};

class Scope {
public:
  using IdentId = unsigned;
  enum Kind { FILE, FUNC, FUNC_PROTOTYPE, BLOCK };
  Scope(Kind kind) : kind(kind) {}

  Symbol *declareSymbol(IdentId identId, Symbol symbol);
  Symbol *getSymbol(IdentId identId);
  Kind getKind() { return kind; }

  bool isFile() { return kind == FILE; };
  bool isBlock() { return kind == BLOCK; };

  Symbol::Kind getSymbolKind(Symbol::Kind symbolKind) {
    if (symbolKind != Symbol::EMPTY) {
      return symbolKind;
    }
    return getDefaultSymbolKind();
  }

  Symbol::Kind getDefaultSymbolKind() {
    switch (kind) {
    case FILE:
      return Symbol::EXTERN;
    case BLOCK:
      return Symbol::AUTO;
    default:
      return Symbol::EMPTY;
    }
  }

private:
  Kind kind;
  std::unordered_map<IdentId, Symbol> symbols;
};

class SymbolTable {
public:
  using IdentId = Scope::IdentId;

  class ScopeHandle {
  public:
    ScopeHandle(SymbolTable &sym) : sym(&sym) {}
    ScopeHandle(const ScopeHandle &) = delete;
    ScopeHandle &operator=(const ScopeHandle &) = delete;
    ~ScopeHandle() {
      if (sym) {
        sym->popScope();
      }
    }

  private:
    SymbolTable *sym;
  };

  SymbolTable() {}
  void pushScope(Scope &scope) { scopes.push_back(&scope); }

  void popScope() {
    assert(scopes.size() > 0);
    scopes.pop_back();
  }

  Scope &scope() {
    assert(scopes.size() > 0);
    return *scopes.back();
  }

  Scope &scope(unsigned n) {
    assert(n < scopes.size());
    return *scopes[n];
  }

  void clearScopeStack() { scopes.clear(); }

  Symbol *declareSymbol(Symbol symbol) {
    symbol.id = nextSymId;
    Symbol *sym =
        scope().declareSymbol(declareIdent(symbol.name), std::move(symbol));
    if (!sym) {
      return nullptr;
    }
    ++nextSymId;
    return sym;
  }

  IdentId declareIdent(std::string_view ident) {
    auto [it, succ] = identIdMap.insert(std::make_pair(ident, nextIdentId));
    if (succ) {
      ++nextIdentId;
    }
    return it->second;
  }

  Symbol *getSymbol(std::string_view name) {
    auto idIt = identIdMap.find(name);
    if (idIt == identIdMap.end())
      return nullptr;
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
      auto *sym = (*it)->getSymbol(idIt->second);
      if (sym)
        return sym;
    }
    return nullptr;
  }

private:
  std::vector<Scope *> scopes;
  std::unordered_map<std::string_view, IdentId> identIdMap;
  IdentId nextIdentId = 1;
  SSASymbolId nextSymId = 1;
};

inline Symbol *Scope::declareSymbol(IdentId identId, Symbol symbol) {
  auto [it, succ] = symbols.try_emplace(identId, std::move(symbol));
  if (succ) {
    return &it->second;
  }
  return nullptr;
}

inline Symbol *Scope::getSymbol(IdentId identId) {
  auto it = symbols.find(identId);
  if (it == symbols.end()) {
    return nullptr;
  }
  return &it->second;
}
}; // namespace c
