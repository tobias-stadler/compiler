#pragma once

#include "c/Type.h"
#include "ir/IR.h"
#include "support/RefCount.h"
#include <unordered_map>

namespace c {

class Symbol {
  friend class SymbolTable;

public:
  enum Kind { EMPTY, TYPE, EXTERN, STATIC, AUTO, REGISTER };
  enum StorageKind { SSA, ALLOCA };
  Symbol(Kind kind, CountedPtr<Type> type)
      : kind(kind), type(std::move(type)) {}

  Kind getKind() { return kind; }

  CountedPtr<Type> &getType() { return type; }

  SSASymbolId getId() { return id; }

  bool isAddrTaken() { return addrTaken; }
  void setAddrTaken(bool taken) { addrTaken = taken; }

private:
  Kind kind;
  CountedPtr<Type> type;
  SSASymbolId id = 0;
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

  Symbol *declareSymbol(std::string_view name, Symbol symbol) {
    symbol.id = nextSymId;
    Symbol *sym = scope().declareSymbol(declareIdent(name), std::move(symbol));
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

class DeclSpec {
public:
  DeclSpec() = default;
  DeclSpec(Symbol::Kind symbolKind, CountedPtr<Type> type,
           Type::Qualifier qualifier)
      : symbolKind(symbolKind), type(std::move(type)), qualifier(qualifier) {}
  Symbol::Kind symbolKind = Symbol::EMPTY;
  CountedPtr<Type> type;
  Type::Qualifier qualifier;
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
