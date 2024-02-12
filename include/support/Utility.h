#pragma once

#include <cassert>

[[noreturn]] inline void unreachable() { __builtin_unreachable(); }

#define UNREACHABLE(msg)                                                       \
  assert(false && msg);                                                        \
  unreachable();
