#pragma once

#include "c/AST.h"
#include "c/Symbol.h"
#include "ir/IR.h"
#include <memory>

namespace c {

std::unique_ptr<Program> IRGenAST(TranslationUnitAST &ast, SymbolTable &sym);

}
