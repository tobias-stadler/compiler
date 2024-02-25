#pragma once

#include "ir/Alignment.h"
#include "ir/Operand.h"
#include <memory>
#include <ranges>
#include <vector>

class FrameDef : public OtherSSADef {
  friend class FrameLayout;

public:
  static bool is_impl(const OtherSSADef &o) {
    return o.getKind() == LOCAL_FRAME;
  }

  FrameDef(int32_t id, size_t size, Alignment align)
      : OtherSSADef(LOCAL_FRAME), id(id), size(size), align(align) {}

  int32_t getId() const { return id; }

  size_t getSize() { return size; }

  Alignment getAlign() { return align; }

private:
  int32_t id;
  size_t size;
  Alignment align;
};

class FrameLayout {
public:
  FrameDef &createFrameEntry(size_t size, Alignment align) {
    auto newEntry = std::make_unique<FrameDef>(entries.size(), size, align);
    FrameDef *entry = newEntry.get();
    entries.push_back(std::move(newEntry));
    return *entry;
  }

  auto entryRange() {
    return entries |
           std::views::transform([](auto &e) -> FrameDef & { return *e; });
  }

  size_t getNumEntries() { return entries.size(); }

private:
  std::vector<std::unique_ptr<FrameDef>> entries;
};
