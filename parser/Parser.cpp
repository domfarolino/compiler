#include "Parser.h"

#include <iostream>
#include <string>
#include <queue>

#include "../lexer/Lexer.h"
#include "../lib/Token.h"

Parser::Parser(Lexer& inLexer): lexer_(inLexer) {
  token_ = lexer_.nextToken();
  std::cout << "Parser is starting up!" << std::endl;

  // Start parsing the program from the <program> production!
  Program();
  FlushErrors();
}

// This function compares the most recently received token (stored in token_)
// with an expected token terminal from some production. Unexpected, but valid
// tokens are not to be specifically handled by this function, as that is the
// responsibility of the caller. Comments should also be entirely handled by
// the lexer, resulting in non-comment tokens appearing here. See
// https://github.com/domfarolino/compiler/issues/3.
bool Parser::CheckTokenType(TokenType expectedTokenType) {
  if (lexer_.isDone()) return false;

  // TODO(domfarolino): Handle invalid tokens, EOF, etc.
  if (token_.type == expectedTokenType) {
    token_ = lexer_.nextToken();
    return true;
  }

  return false;
}

// Queues a plain error message to the error queue, for later displaying.
void Parser::QueueError(std::string inErrorString) {
  errorQueue_.push(inErrorString);
}

void Parser::FlushErrors() {
  std::cerr << "\033[1;31m";

  while (!errorQueue_.empty()) {
    std::cerr << errorQueue_.front() << std::endl;
    errorQueue_.pop();
  }

  std::cerr << "\033[0m";
}

// <program> ::= <program_header> <program_body> .
void Parser::Program() {
  bool parsedHeader = ProgramHeader();
  if (!parsedHeader)
    QueueError("Error parsing program header");

  bool parsedBody = ProgramBody();
  if (!parsedBody)
    QueueError("Error parsing program body");

  if (!CheckTokenType(TokenType::TPeriod))
    QueueError("Missing period ('.') at the end of program");
}

// <program_header> ::= program <identifier> is
bool Parser::ProgramHeader() {
  // TODO(domfarolino): Do this.
  return true;
}

// <program_body> ::= ( <declaration> ; )* begin ( <statement> ; )* end program
bool Parser::ProgramBody() {
  // TODO(domfarolino): Do this.
  return true;
}
