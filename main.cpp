#include <iostream>
#include "lexer/Lexer.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "You must provide exactly one argument (an input program) to the compiler" << std::endl;
    return 1;
  }

  Lexer lexer(argv[1]);
  while (!lexer.isDone()) lexer.nextToken();

  return 0;
}
