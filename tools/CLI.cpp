#include "Lexer.h"
#include "Parser.h"
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
    AST *ast = p.parseStatement();
    if (!ast) {
      std::cout << "Parsing failed\n";
      continue;
    }
    std::cout << "Parsing finished\n";
    PrintASTVisitor().run(ast);
    delete ast;
  }
  return 0;
}
