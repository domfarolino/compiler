#include "Parser.h"

#include <iostream>
#include <string>
#include <queue>

#include "../lexer/Lexer.h"
#include "../lib/Token.h"

/**
 * This is the parser. It is an LL(1) recursive decent parser, though it
 * technically does not need to be recursive, given the LL(1) nature of the
 * grammar. Currently, production methods return true if a production existed
 * without errors, and false if it started to exist but failed parsing, or did
 * not exist at all. This is alright for the current error-handling mechanism,
 * which basically dies upon seeing the first error, and stops parsing.
 * Eventually we should have more advanced error-handling and resynchronization,
 * and keep parsing until we absolutely cannot. In this case, we'll want the
 * production methods to return true if they exist with or with out errors, and
 * false only if they don't exist. This is because we want to continue parsing
 * even in the midst of errors, and only want to stop if we cannot get
 * resynchronized. The resync logic will be separate, and is necessary to parse
 * even in the midst of errors, to stop errors from becoming incredibly verbose
 * if the parser cannot find a good starting spot after an error state. Since
 * the resync logic is not implemented yet, we don't parse in the midst of
 * errors. If our production methods returned true even when errors were found
 * and we wanted to stop parsing in the midst of errors, we'd have to do an
 * error check after ever single production method call. That's a lot of checks.
 * Therefore, it seemed easier to just make erroneous parses return false, since
 * in the case a production was required, erroneous and nonexistant would be
 * handled the same. In the case where a production was not required, we just
 * have to check to see if the method returned false because of an error, or
 * because it did not exist. This seems like less checks, but I could be wrong.
 */

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

void Parser::QueueExpectedTokenError(std::string inErrorString) {
  errorQueue_.push("Line " + std::to_string(lexer_.lineNumber) + ": " + inErrorString + ", but got: '" + token_.lexeme + "'");
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
  else if (parsedHeader && parsedBody)
    std::cout << "Program compiled successfully" << std::endl;
}

// <program_header> ::= program <identifier> is
bool Parser::ProgramHeader() {
  if (!CheckTokenType(TokenType::TProgram)) {
    QueueExpectedTokenError("Expected 'program' in program header");
    return false;
  }

  // program
  if (!Identifier()) {
    QueueExpectedTokenError("Expected program identifier in program header");
    return false;
  }

  // program <identifier>
  if (!CheckTokenType(TokenType::TIs)) {
    QueueExpectedTokenError("Expected 'is' in program header");
    return false;
  }

  // program <identifier> is
  return true;
}

// <program_body> ::= (<declaration>;)* begin (<statement>;)* end program
bool Parser::ProgramBody() {
  int errorQueueSizeSnapshot = errorQueue_.size();
  while (Declaration()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after declaration");
      return false;
    }
  }

  // Declaration() isn't required, so there are two reasons it may return false:
  //  1.) The first token didn't match what we were looking for, so we have to
  //      assume the user provided no declaration. That's ok, we should only
  //      return false here if a given declaration _failed_ to parse part way
  //      through.
  //  2.) We were part-way through parsing a declaration and failed for some
  //      very bad reason, in which case the program body also failed to parse.
  //      We can distinguish between missing and invalid optional productions
  //      by taking a snapshot of the error queue.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (!CheckTokenType(TokenType::TBegin)) {
    QueueExpectedTokenError("Expected 'begin' in program body");
    return false;
  }

  // For now we're only doing declarations
  /*
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement");
      return false;
    }
  }
  */

  if (!CheckTokenType(TokenType::TEnd) ||
      !CheckTokenType(TokenType::TProgram)) {
    QueueExpectedTokenError("Expected 'end program' after program body");
    return false;
  }

  return true;
}

// <identifier> ::= [a-zA-Z][a-zA-Z0-9_]*
bool Parser::Identifier() {
  // TODO(domfarolino): We probably want to return the token here too later.
  // TODO(domfarolino): Maybe if the token was not an identifier, we can check
  // to see if it is a reserved word and throw a more specific error, like "can
  // not use reserved word as identifier" or something.
  return CheckTokenType(TokenType::TIdentifier);
}

// <declaration> ::= [ global ] <procedure_declaration> | [ global ] <variable_declaration>
bool Parser::Declaration() {
  bool global = false;
  if (CheckTokenType(TokenType::TGlobal))
    global = true;

  int errorQueueSizeSnapshot = errorQueue_.size();
  if (ProcedureDeclaration()) {
    // TODO(domfarolino): Symbol management for procedure declaration.
    return true;
  } else if (errorQueue_.size() == errorQueueSizeSnapshot && VariableDeclaration()) {
    // TODO(domfarolino): Symbol management for variable declaration.
    return true;
  }

  // At this point, there was either an error parsing one of them, or they both
  // did not exist. Either way, we return false so the caller knows it shouldn't
  // look for a semicolon. The caller will determine whether or not it should
  // continue parsing given the error queue size snapshot.
  if (global)
    QueueExpectedTokenError("Expected procedure or variable declaration after 'global'");

  return false;
}

/*
<statement> ::= <assignment_statement> |
                <if_statement>         |
                <loop_statement>       |
                <return_statement>     |
                <procedure_call>
bool Parser::Statement() {
  return true;
}
*/

// <procedure_declaration> ::= <procedure_header> <procedure_body>
bool Parser::ProcedureDeclaration() {
  // This is not a valid ProcedureDeclaration, is the ProcedureHeader is either
  // missing or invalid.
  if (!ProcedureHeader())
    return false;

  // <procedure_header>
  // Same goes for ProcedureBody...
  return ProcedureBody();
}

// <procedure_header> :: = procedure <identifier> ( [<parameter_list>] )
bool Parser::ProcedureHeader() {
  // A <procedure_header> doesn't always have to exist (as up the chain it is
  // eventually a (declaration*), which can appear no times), so if the first
  // terminal of a production like this, we have to let our caller decide
  // whether it is appropriate to throw an error. This is very similar to what
  // we do in VariableDeclaration, since it seeks for the first terminal in a
  // production that doesn't always have to produce anything.
  if (!CheckTokenType(TokenType::TProcedure))
    return false;

  // "procedure"
  if (!Identifier()) {
    QueueExpectedTokenError("Expected identifier in procedure header");
    return false;
  }

  // "procedure <identifier>"
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' before parameter list in procedure header");
    return false;
  }

  // "procedure <identifier>("
  // A ParameterList is optional, so if ParameterList() returned false only
  // because it didn't exist, we can't let that make us return false here, since
  // the rest of ProcedureHeader existed just fine. We only want to propagate
  // errors.
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ParameterList() && errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // "procedure <identifier>(..."
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after parameter list in procedure header");
    return false;
  }

  // "procedure <identifier>(...)
  return true;
}

// <parameter_list> ::= <parameter> , <parameter_list> | <parameter>
bool Parser::ParameterList() {
  // The language allows empty parameter lists, so Parameter() could have
  // returned false because it didn't exist, or because it failed to parse.
  // Either way, we return false, and our caller will detect whether there
  // was an error or not. If Parameter() was true, we only return true if
  // there are no trailing commas in the list.
  if (!Parameter())
    return false;

  while (CheckTokenType(TokenType::TComma)) {
    if (!Parameter()) {
      QueueError("Expected parameter after ',' in parameter list");
      return false;
    }
  }

  return true;
}

// <parameter> ::= <variable_declaration> (in | out | inout)
bool Parser::Parameter() {
  // If VD returns false, that could be because it didn't find a VD, or because
  // a VD failed to parse. Parameters aren't mandatory, so we only care about
  // the latter case. Our caller will determine whether the VD had an error
  // parsing or not. In this frame, we only care about completion.
  if (!VariableDeclaration())
    return false;

  if (!CheckTokenType(TokenType::TIn) &&
      !CheckTokenType(TokenType::TOut) &&
      !CheckTokenType(TokenType::TInOut)) {
    QueueExpectedTokenError("Expected 'in', 'out', or 'inout' after parameter identifier");
    return false;
  }

  return true;
}

// <procedure_body> ::= (<declaration>;)* begin (<statement>;)* end procedure
bool Parser::ProcedureBody() {
  return true;
}

// <variable_declaration> ::= <type_mark> <identifier> [ [ <lower_bound> “:” <upper_bound> ] ]
bool Parser::VariableDeclaration() {
  // Can't report error if TypeMark was not found, because VariableDeclaration
  // isn't always required. Caller will decide whether a VD that doesn't exist
  // is an error or not, given the context.
  if (!TypeMark())
    return false;

  // <type_mark>
  if (!Identifier()) {
    QueueExpectedTokenError("Expected identifier in variable declaration");
    return false;
  }

  // <type_mark> <identifier>
  if (CheckTokenType(TokenType::TLeftBracket)) {

    // Declaring an array, lower and upper bound are required.
    // <type_mark> <identifier> [
    if (!LowerOrUpperBound()) {
      QueueExpectedTokenError("Expected number for array lower bound");
      return false;
    }

    // <type_mark> <identifier> [ <number>
    if (!CheckTokenType(TokenType::TColon)) {
      QueueExpectedTokenError("Expected ':' separating lower and upper array bounds");
      return false;
    }

    // <type_mark> <identifier> [ <number> :
    if (!LowerOrUpperBound()) {
      QueueExpectedTokenError("Expected number for array upper bound");
      return false;
    }

    // <type_mark> <identifier> [ <number> : <number>
    if (!CheckTokenType(TokenType::TRightBracket)) {
      QueueExpectedTokenError("Expected ']' after array bounds");
      return false;
    }
  }

  return true;
}

// <type_mark> ::= integer | float | string | bool | char
// Not necessary to find one in all usages, so we leave error reporting to the
// caller.
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
// Not necessary to find one in all usages, so we leave error reporting to the
// caller.
bool Parser::Number() {
  return CheckTokenType(TokenType::TInteger) ||
         CheckTokenType(TokenType::TFloat);
}
