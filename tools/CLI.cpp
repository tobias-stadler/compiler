#include <frontend/AST.h>
#include <frontend/Lexer.h>
#include <frontend/Parser.h>
#include <fstream>
#include <iostream>
#include <string>

int main() {
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
    auto ast = p.parseDeclarator(true);
    if (!ast) {
      std::cout << "Parsing failed\n";
      continue;
    }
    std::cout << "Parsing finished\n";
    PrintASTVisitor().run(&ast.res());
  }
  return 0;
}
