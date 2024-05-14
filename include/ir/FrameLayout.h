#pragma once

#include "ir/Alignment.h"
#include "ir/Operand.h"
#include <memory>
#include <ranges>
#include <vector>

class FrameDef : public ExternSSADef {
  friend class FrameLayout;

public:
  static bool is_impl(const ExternSSADef &o) {
    return o.getKind() == FUNC_FRAME;
  }

  FrameDef(int32_t id, size_t size, Alignment align)
      : ExternSSADef(FUNC_FRAME), id(id), size(size), align(align) {}

  size_t getId() const { return id; }

  size_t getSize() { return size; }

  Alignment getAlign() { return align; }

private:
  size_t id;
  size_t size;
  Alignment align;
};

class FrameLayout {
public:
  FrameDef &createFrameEntry(size_t size, Alignment align) {
    auto newEntry =
        std::make_unique<FrameDef>(entryStorage.size(), size, align);
    FrameDef *entry = newEntry.get();
    entryStorage.push_back(std::move(newEntry));
    return *entry;
  }

  auto entries() {
    return entryStorage |
           std::views::transform([](auto &e) -> FrameDef & { return *e; });
  }

  size_t getNumEntries() { return entryStorage.size(); }

private:
  std::vector<std::unique_ptr<FrameDef>> entryStorage;
};
