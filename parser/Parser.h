#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <queue>

#include "../lib/Token.h"

class Lexer;

class Parser {
public:
  Parser(Lexer&);

private:
  // Members
  Lexer& lexer_;
  Token token_;
  std::queue<std::string> errorQueue_;

  // Utility Methods
  bool CheckTokenType(TokenType);
  void QueueError(std::string);
  void FlushErrors();

  // Productions
  void Program();
  bool ProgramHeader();
  bool ProgramBody();
};

#endif
