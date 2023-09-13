#pragma once

#include "frontend/AST.h"
#include "frontend/Symbol.h"
#include "ir/IR.h"
#include <memory>

std::unique_ptr<Program> IRGenAST(TranslationUnitAST &ast, SymbolTable &sym);
