#include "ir/IRPatExecutor.h"
#include <c/AST.h>
#include <c/ASTPrinter.h>
#include <c/IRGen.h>
#include <c/Lexer.h>
#include <c/Parser.h>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <ir/DominatorTree.h>
#include <ir/IR.h>
#include <ir/IRPass.h>
#include <ir/IRPrinter.h>
#include <ir/InstrBuilder.h>
#include <ir/PhiIsolation.h>
#include <ir/RegLiveness.h>
#include <ir/RegTracking.h>
#include <memory>
#include <riscv/Arch.h>
#include <string>

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Expected filename" << std::endl;
    return EXIT_FAILURE;
  }
  std::ifstream f(argv[1], std::ios::ate);
  if (!f.good()) {
    std::cerr << "Couldn't open file" << std::endl;
    return EXIT_FAILURE;
  }

  auto sz = f.tellg();
  f.seekg(0);
  std::string str;
  str.resize(sz);
  f.read(str.data(), sz);

  c::Lexer lex(str);
  c::SymbolTable sym;
  c::Parser p(lex, sym);

  auto ast = p.parseTranslationUnit();
  if (!ast) {
    std::cout << "Parsing failed\n";
    return EXIT_FAILURE;
  }
  std::cout << "Parsing finished\n";
  PrintAST(**ast);

  auto prog = IRGenAST(**ast, sym);
  auto &func = *prog->functions[0];

  riscv::Arch arch;
  IRPipeline<Function> pipeline(&arch);

  pipeline.addLazyPass(std::make_unique<DominatorTreePass>());
  pipeline.addLazyPass(std::make_unique<LivenessFlowPass>());
  pipeline.addLazyPass(std::make_unique<LiveIntervalsPass>());

  pipeline.addPass(std::make_unique<PrintIRPass>());
  // pipeline.addPass(std::make_unique<PrintIRPass>());
  // pipeline.addPass(std::make_unique<PrintDominatorTreePass>());

  riscv::PreISelExpansion expansion;
  riscv::PreISelCombine combine;
  riscv::InstrSelect isel;
  pipeline.addPass(std::make_unique<InstrExpansionPass>(expansion));
  pipeline.addPass(std::make_unique<InstrCombinePass>(combine));
  pipeline.addPass(std::make_unique<InstrSelectPass>(isel));
  pipeline.addPass(std::make_unique<riscv::BranchLoweringPass>());
  pipeline.addPass(std::make_unique<PrintIRPass>());
  pipeline.addPass(std::make_unique<PhiIsolationPass>());
  pipeline.addPass(std::make_unique<RegTrackingPass>());
  pipeline.addPass(std::make_unique<PhiDestructionPass>());

  pipeline.addPass(std::make_unique<PrintIRPass>());
  pipeline.addPass(std::make_unique<PrintRegTrackingPass>());
  pipeline.addPass(std::make_unique<PrintLivenessFlowPass>());
  pipeline.addPass(std::make_unique<PrintLiveIntervalsPass>());

  pipeline.run(func);
  return EXIT_SUCCESS;
}
