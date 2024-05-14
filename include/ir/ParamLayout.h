#pragma once

#include "ir/Operand.h"
#include <memory>
#include <ranges>
#include <vector>

class ParamDef : public ExternSSADef {
  friend class FrameLayout;

public:
  static bool is_impl(const ExternSSADef &o) {
    return o.getKind() == FUNC_PARAM;
  }

  ParamDef(int32_t id) : ExternSSADef(FUNC_PARAM), id(id) {}

  size_t getId() const { return id; }

private:
  size_t id;
};

class ParamLayout {
public:
  ParamDef &createParam() {
    auto newEntry = std::make_unique<ParamDef>(entries.size());
    ParamDef *entry = newEntry.get();
    entries.push_back(std::move(newEntry));
    return *entry;
  }

  auto entryRange() {
    return entries |
           std::views::transform([](auto &e) -> ParamDef & { return *e; });
  }

  size_t getNumEntries() { return entries.size(); }

private:
  std::vector<std::unique_ptr<ParamDef>> entries;
};
