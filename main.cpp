#include <iostream>
#include "lexer/Lexer.h"
#include "scope/ScopeManager.h"
#include "parser/Parser.h"

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << "You must provide one argument (an input program) to the compiler" << std::endl;
    return 1;
  }

  // Command line args can change these, limiting which phases of compiler run.
  bool lexerOnly = false;
  bool verbose = false;
  bool symbolInsight = false;

  // Process potential command line arguments.
  if (argc >= 3) {
    int i = 2;
    while (i < argc) {
      if (strcmp(argv[i], "--lexer-only") == 0) {
        lexerOnly = true;
        verbose = true;
      } else if (strcmp(argv[i], "--verbose") == 0) {
        verbose = true;
      } else if (strcmp(argv[i], "--symbol-insight") == 0) {
        symbolInsight = true;
      }

      i++;
    }
  }

  Lexer lexer(argv[1], verbose);
  ScopeManager scopeManager;

  if (lexerOnly) {
    std::cout << "\033[1;34mStarting up in --lexer-only mode! \033[0m" << std::endl;
    while (!lexer.isDone()) lexer.nextToken();
  } else {
    Parser parser(lexer, scopeManager, symbolInsight);
  }

  return 0;
}
