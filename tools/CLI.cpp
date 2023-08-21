#include <cstdlib>
#include <frontend/AST.h>
#include <frontend/ASTPrinter.h>
#include <frontend/Lexer.h>
#include <frontend/Parser.h>
#include <fstream>
#include <iostream>
#include <ir/IR.h>
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
  PrintAST(ast->get());

  Block b;
  InstrEmitter e(b);
  auto i1 = e.emitConstInt(IntSSAType::get(32), 1);
  auto i2 = e.emitConstInt(IntSSAType::get(32), 2);
  auto i3 = e.emitAdd(IntSSAType::get(32), i1, i2);
  i1.unlinkAndDelete();
  i2.unlinkAndDelete();
  i3.unlinkAndDelete();


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
    PrintAST(ast->get());
  }
  return;
}
