#pragma once

#include "frontend/AST.h"
#include "ir/IR.h"
#include <memory>

std::unique_ptr<Program> IRGenAST(AST &ast);
