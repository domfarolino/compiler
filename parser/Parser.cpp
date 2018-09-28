#include "Parser.h"

#include <iostream>
#include <string>
#include <queue>

#include "../lexer/Lexer.h"
#include "../lib/Token.h"

Parser::Parser(Lexer& inLexer): lexer_(inLexer) {
  token_ = lexer_.nextToken();

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
  // ...

  if (token_.type == expectedTokenType) {
    token_ = lexer_.nextToken();
    return true;
  }

  return false;
}

// Queues a plain error message to the error queue, for later displaying.
void Parser::QueueError(std::string inErrorString) {
  errorQueue_.push("Line " + std::to_string(lexer_.lineNumber) + ": " + inErrorString);
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
  if (!parsedBody) {
    QueueError("Error parsing program body");
    return;
  }

  if (!CheckTokenType(TokenType::TPeriod))
    QueueError("Missing period ('.') at the end of program");
  else
    std::cout << "Program compiled successfully" << std::endl;
}

// <program_header> ::= program <identifier> is
bool Parser::ProgramHeader() {
  if (!CheckTokenType(TokenType::TProgram)) {
    QueueError("Missing 'program' statement");
    return false;
  }

  // program
  if (!Identifier()) {
    QueueError("Error parsing identifier in program header");
    return false;
  }

  // program <identifier>
  if (!CheckTokenType(TokenType::TIs)) {
    QueueError("Missing 'is' in program header");
    return false;
  }

  // program <identifier> is
  return true;
}

// <program_body> ::= (<declaration>;)* begin (<statement>;)* end program
bool Parser::ProgramBody() {
  while (Declaration()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueError("Expected ';' after declaration");
      return false;
    }
  }

  if (!CheckTokenType(TokenType::TBegin)) {
    QueueError("Expected 'begin' in program body");
    return false;
  }

  // For now we're only doing declarations
  /*
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueError("Expected ';' after statement");
      return false;
    }
  }
  */

  if (!CheckTokenType(TokenType::TEnd) ||
      !CheckTokenType(TokenType::TProgram)) {
    QueueError("Expected 'end program' after program body");
    return false;
  }

  return true;
}

// <identifier> ::= [a-zA-Z][a-zA-Z0-9_]*
bool Parser::Identifier() {
  // TODO(domfarolino): We probably want to return the token here too later.
  return CheckTokenType(TokenType::TIdentifier);
}

// <declaration> ::= [ global ] <procedure_declaration> | [ global ] <variable_declaration>
bool Parser::Declaration() {
  bool global = false;
  if (CheckTokenType(TokenType::TGlobal))
    global = true;

  if (/*!ProcedureDeclaration() && */!VariableDeclaration()) {
    if (global)
      QueueError("Expected procedure or variable declaration after 'global'");
    return false;
  }

  return true;
}

/*
<statement> ::= <assignment_statement> |
                <if_statement> |
                <loop_statement> |
                <return_statement> |
                <procedure_call>
bool Parser::Statement() {
  return true;
}

// <procedure_declaration> ::= <procedure_header> <procedure_body>
bool Parser::ProcedureDeclaration() {
  return true;
}
*/

// <variable_declaration> ::= <type_mark> <identifier> [ [ <lower_bound> “:” <upper_bound> ] ]
bool Parser::VariableDeclaration() {
  // Not every usage of <variable_declaration> requires at least one variable
  // declaration, so we can't queue an error here if we fail to retrieve a type
  // mark. The caller must decide what to do. // TODO(domfarolino): Consider
  // making this function aware of whether it is required to produce something,
  // so we can report errors for TypeMark() failures when one is necessary.
  if (!TypeMark())
    return false;

  if (!Identifier()) {
    QueueError("Error parsing identifier in variable declaration");
    return false;
  }

  if (CheckTokenType(TokenType::TLeftBracket)) {
    // Declaring an array, lower and upper bound are required.

    if (!LowerOrUpperBound()) {
      QueueError("Error parsing array bound");
      return false;
    }

    if (!CheckTokenType(TokenType::TColon)) {
      QueueError("Expected ':' separating lower and upper array bounds");
      return false;
    }

    if (!LowerOrUpperBound()) {
      QueueError("Error parsing array bound");
      return false;
    }

    if (!CheckTokenType(TokenType::TRightBracket)) {
      QueueError("Expected ']' after array bounds");
      return false;
    }
  }

  return true;
}

// <type_mark> ::= integer | float | string | bool | char
bool Parser::TypeMark() {
  return CheckTokenType(TokenType::TIntegerType) ||
         CheckTokenType(TokenType::TFloatType)   ||
         CheckTokenType(TokenType::TStringType)  ||
         CheckTokenType(TokenType::TBoolType)    ||
         CheckTokenType(TokenType::TCharType);
}

// <lower_bound> ::= [-] <number>
// <upper_bound> ::= [-] <number>
bool Parser::LowerOrUpperBound() {
  bool isNegative = false;

  if (CheckTokenType(TokenType::TMinus))
    isNegative = true;

  return Number();
}

// <number> ::= [0-9][0-9_]*[.[0-9_]*]
bool Parser::Number() {
  return CheckTokenType(TokenType::TInteger) ||
         CheckTokenType(TokenType::TFloat);
}
