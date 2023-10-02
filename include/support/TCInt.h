#pragma once

#include <cstdint>

class TCInt {
public:
  template <unsigned N> static bool canTrunc(int32_t v) {
    return (v < (INT32_C(1) << (N - 1))) && (v >= -(INT32_C(1) << (N - 1)));
  }
};
