#include <iostream>
#include <fstream>

#include "Token.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "You must provide exactly one argument (an input program) to the compiler" << std::endl;
    return 1;
  }

  std::ifstream source;
  source.open(argv[1], std::ifstream::in);
  if (!source.is_open()) {
    std::cerr << "Failed to open " << argv[1] << std::endl;
  }

  /*
   * Lexing begins here. TODO(domfarolino): eventually we'll want
   * to factor this out into a Lexer class that can output a single
   * token at a time.
   */
  std::string input;
  while (1) {
    // Get input full token type
    Token token(TokenType::TColon, ":");
    break;
  }

  source.close();
  return 0;
}
