#include "frontend/IRGen.h"
#include "frontend/ASTVisitor.h"

namespace {

class IRGenASTVisitor : public ASTVisitor<IRGenASTVisitor> {};
} // namespace
