#include "Lexer.h"
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
  }
  return 0;
}
