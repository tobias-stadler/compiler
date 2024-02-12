#pragma once

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

  FrameDef(int32_t id, SSAType &elementType, size_t numElements)
      : OtherSSADef(LOCAL_FRAME), id(id), elementType(elementType),
        numElements(numElements) {}

  int32_t getId() const { return id; }

  SSAType &getElementType() const { return elementType; }

  bool isSingle() const { return numElements == 1; }

  size_t getNumElements() const { return numElements; }

private:
  int32_t id;
  SSAType &elementType;
  size_t numElements;
};

class FrameLayout {
public:
  FrameDef &createFrameEntry(SSAType &ty, size_t numElements) {
    auto newEntry = std::make_unique<FrameDef>(entries.size(), ty, numElements);
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
