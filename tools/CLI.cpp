#include <cstdlib>
#include <frontend/AST.h>
#include <frontend/ASTPrinter.h>
#include <frontend/IRGen.h>
#include <frontend/Lexer.h>
#include <frontend/Parser.h>
#include <fstream>
#include <iostream>
#include <ir/IR.h>
#include <ir/IRPrinter.h>
#include <ir/InstrBuilder.h>
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

  Lexer lex(str);
  Parser p(&lex);

  auto ast = p.parseTranslationUnit();
  if (!ast) {
    std::cout << "Parsing failed\n";
    return EXIT_FAILURE;
  }
  std::cout << "Parsing finished\n";
  PrintAST(**ast);

  auto prog = IRGenAST(**ast);
  PrintIR(*prog);

  auto& t = VoidSSAType::get();

  /*
  Function func;
  Block *b = new Block();
  Block *b2 = new Block();
  func.insertBegin(b);
  func.insertEnd(b2);
  InstrBuilder e2(*b2);
  auto &i4 = e2.emitConstInt(IntSSAType::get(32), 4);
  InstrBuilder e1(*b);
  auto &i1 = e1.emitConstInt(IntSSAType::get(32), 1);
  auto &i2 = e1.emitConstInt(IntSSAType::get(32), 2);
  auto &i3 = e1.emitAdd(i1.getDef(), i2.getDef());
  auto &i5 = e1.emitCmp(BrCond::eq(), i3.getDef(), i4.getDef());
  e1.emitBrCond(i5.getDef(), *b2, *b);
  PrintIR(func);
  */

  return EXIT_SUCCESS;
}

void interactive() {
  std::cout << "Hello\n";
  std::string line;
  while (std::getline(std::cin, line)) {
    Lexer lex{line};
    Token tok;
    while (!(tok = lex.nextToken()).isEnd()) {
      std::cout << tok << std::endl;
    }
    Lexer lexP{line};
    Parser p{&lexP};
    auto ast = p.parseTranslationUnit();
    if (!ast) {
      std::cout << "Parsing failed\n";
      continue;
    }
    std::cout << "Parsing finished\n";
    PrintAST(*ast.res());
  }
  return;
}
