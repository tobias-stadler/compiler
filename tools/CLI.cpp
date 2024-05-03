#include "ir/IRPatExecutor.h"
#include "ir/RegAlloc.h"
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
#include <riscv/InstrSelect.h>
#include <riscv/PreISelCombine.h>
#include <riscv/PreISelExpansion.h>
#include <string>

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Expected filename" << std::endl;
    return EXIT_FAILURE;
  }
  c::IncludeLexer incLex;
  incLex.basePaths.push_back("/usr/include");
  incLex.includePath(argv[1]);

  c::ASTContext ctx;
  c::PPSymbolTable ppSym;
  c::PPLexer ppLex(ctx, incLex, ppSym);
  c::SymbolTable sym;
  c::Parser p(ctx, ppLex, sym);

  auto ast = p.parseTranslationUnit();
  if (!ast || ppLex.peekKind() != c::Token::END) {
    p.printErrCtx(std::cerr);
    return EXIT_FAILURE;
  }
  std::cout << "Parsing finished\n";
  PrintAST(**ast);

  auto prog = IRGenAST(**ast, ctx, sym);

  riscv::Arch arch;
  IRPipeline<Program> progPipeline(&arch);
  IRPipeline<Function> funcPipeline(&arch);

  riscv::PreISelExpansion expansion;
  riscv::PreISelCombine combine;
  riscv::InstrSelect isel;

  funcPipeline.addLazyPass(std::make_unique<DominatorTreePass>());
  funcPipeline.addLazyPass(std::make_unique<LiveFlowPass>());
  funcPipeline.addLazyPass(std::make_unique<LiveIntervalsPass>());
  funcPipeline.addLazyPass(std::make_unique<LiveGraphPass>());

  funcPipeline.addPass(std::make_unique<PrintIRPass>());
  // pipeline.addPass(std::make_unique<PrintDominatorTreePass>());
  bool doISel = true;
  bool doRegAlloc = true;
  bool doAsmPrint = false;
  if (doISel) {
    funcPipeline.addPass(std::make_unique<riscv::ABILoweringPass>());
    funcPipeline.addPass(std::make_unique<PrintIRPass>());
    funcPipeline.addPass(std::make_unique<InstrExpansionPass>(expansion));
    funcPipeline.addPass(std::make_unique<InstrCombinePass>(combine));
    funcPipeline.addPass(std::make_unique<PrintIRPass>());
    funcPipeline.addPass(std::make_unique<InstrSelectPass>(isel));
    funcPipeline.addPass(std::make_unique<PrintIRPass>());
  }
  if (doRegAlloc && doISel) {
    funcPipeline.addPass(std::make_unique<PhiIsolationPass>());
    funcPipeline.addPass(std::make_unique<RegTrackingPass>());
    funcPipeline.addPass(std::make_unique<PhiDestructionPass>());

    funcPipeline.addPass(std::make_unique<PrintIRPass>());
    funcPipeline.addPass(std::make_unique<PrintRegTrackingPass>());
    funcPipeline.addPass(std::make_unique<PrintLiveGraphPass>());
    // funcPipeline.addPass(std::make_unique<PrintLiveIntervalsPass>());
    // funcPipeline.addPass(std::make_unique<RegAllocPass>());
    // funcPipeline.addPass(std::make_unique<RegRewritePass>());
    // funcPipeline.addPass(std::make_unique<riscv::FrameLoweringPass>());
    // funcPipeline.addPass(std::make_unique<riscv::PostRALowering>());
    // funcPipeline.addPass(std::make_unique<PrintIRPass>());
  }

  progPipeline.addPass(std::make_unique<FunctionPipelinePass>(funcPipeline));

  if (doRegAlloc && doISel && doAsmPrint) {
    progPipeline.addPass(std::make_unique<riscv::AsmPrinterPass>(std::cout));
  }

  progPipeline.run(*prog);

  return EXIT_SUCCESS;
}
