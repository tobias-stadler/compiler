#pragma once

#include "ir/IR.h"

namespace ir_match {

inline bool instrKind(Instr &instr, unsigned kind) {
  return instr.getKind() == kind;
}

inline bool opDefType(Operand &op, SSAType &ty) {
  return op.ssaDefType() == ty;
}


} // namespace ir_match
