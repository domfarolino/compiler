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

  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement");
      return false;
    }
  }

  // Same error checking as above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

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

// <string> :: = "[a-zA-Z0-9 _,;:.']*"
bool Parser::String() {
  return CheckTokenType(TokenType::TString);
}

// <char> ::= '[a-zA-Z0-9 _;:."]'
bool Parser::Char() {
  return CheckTokenType(TokenType::TChar);
}

// <name> ::= <identifier> [ [ <expression> ] ]
// This is identical to Destination(). For now I'm keeping the duplication, in
// case one changes from the other significantly during code-gen perhaps.
bool Parser::Name() {
  // <name> is not required (see <factor>).
  if (!Identifier())
    return false;

  // <identifier>
  if (CheckTokenType(TokenType::TLeftBracket)) {

    // <identifier> [
    if (!Expression()) {
      QueueExpectedTokenError("Expected expression after '[' in name");
      return false;
    }

    // <identifier> [ <expression>
    if (!CheckTokenType(TokenType::TRightBracket)) {
      QueueExpectedTokenError("Expected ']' after expression in name");
      return false;
    }
  }

  // <identifier> [ <expression> ]
  return true;
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

// <statement> ::= <assignment_statement> |
//                 <if_statement>         |
//                 <loop_statement>       |
//                 <return_statement>     |
//                 <procedure_call>
bool Parser::Statement() {
  return AssignmentStatement() || LoopStatement() || IfStatement();
}

// <assignment_statement> ::= <destination> := <expression>
bool Parser::AssignmentStatement() {
  // <assignment_statement> is not required, as up the chain it is part of a
  // (<statement>;)*, so if we fail to find the first token we cannot queue
  // an error quite yet.
  if (!Destination())
    return false;

  // <destination>
  if (!CheckTokenType(TokenType::TColonEq)) {
    QueueExpectedTokenError("Expected ':=' after destination in assignment statement");
    return false;
  }

  int errorQueueSizeSnapshot = errorQueue_.size();
  // <destination> :=
  if (!Expression()) {
    // Only queue an error if Expression didn't already (sometimes it does!).
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected expression for destination in assignment statement");
    return false;
  }

  // <destination> := <expression>
  return true;
}

// <destination> ::= <identifier> [ [ <expression> ] ]
bool Parser::Destination() {
  // <destination> is not required (see <assignment_statement>).
  if (!Identifier())
    return false;

  // <identifier>
  if (CheckTokenType(TokenType::TLeftBracket)) {

    // <identifier> [
    if (!Expression()) {
      QueueExpectedTokenError("Expected expression after '[' in assignment statement");
      return false;
    }

    // <identifier> [ <expression>
    if (!CheckTokenType(TokenType::TRightBracket)) {
      QueueExpectedTokenError("Expected ']' after expression in assignment destination");
      return false;
    }
  }

  // <identifier> [ <expression> ]
  return true;
}

/////////////////////////////// Expression ////////////////////////////////////

// <expression> ::= [ not ] <arithOp> <expressionPrime>
// Expression is not always required, so this function will only queue errors
// when something bad happens, not just for non-existence.
bool Parser::Expression() {
  bool notIsPresent = false;
  if (CheckTokenType(TokenType::TNot))
    notIsPresent = true;

  // [ not ]
  if (!ArithOp())
    return false;

  // [ not ] <arithOp>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ExpressionPrime()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression. Expected valid syntax");
    return false;
  }

  // [ not ] <arithOp> <expressionPrime>
  return true;
}

// <expressionPrime> ::= & <arithOp> <expressionPrime> |
//                       | <arithOp> <expressionPrime> |
//                       ε
bool Parser::ExpressionPrime() {
  if (CheckTokenType(TokenType::TAmp) || CheckTokenType(TokenType::TOr)) {
    // & or |
    if (!ArithOp())
      return false;

    // & or | <arithOp>
    return ExpressionPrime();
  }

  // ε
  return true;
}

//////////////////////////// End Expression ////////////////////////////////

/////////////////////////////// ArithOp ////////////////////////////////////

// <arithOp> ::= <relation> <arithOpPrime>
bool Parser::ArithOp() {
  if (!Relation())
    return false;

  // <relation>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ArithOpPrime()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression operator. Expected valid syntax");
    return false;
  }

  // <relation> <arithOpPrime>
  return true;
}

// <arithOpPrime> ::= + <relation> <arithOpPrime> | - <relation> <arithOpPrime> | ε
bool Parser::ArithOpPrime() {
  if (CheckTokenType(TokenType::TPlus) || CheckTokenType(TokenType::TMinus)) {
    // + or -
    if (!Relation())
      return false;

    // + or - <relation>
    return ArithOpPrime();
  }

  // ε
  return true;
}

///////////////////////////// End ArithOp //////////////////////////////////

////////////////////////////// Relation ////////////////////////////////////

// <relation> ::= <term> <relationPrime>
bool Parser::Relation() {
  if (!Term())
    return false;

  // <term>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!RelationPrime()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression relation. Expected valid syntax");
    return false;
  }

  // <term> <relationPrime>
  return true;
}

// <relationPrime> ::= < <term> <relationPrime> |
//                    >= <term> <relationPrime> |
//                    <= <term> <relationPrime> |
//                     > <term> <relationPrime> |
//                    == <term> <relationPrime> |
//                    != <term> <relationPrime> | ε
bool Parser::RelationPrime() {
  if (CheckTokenType(TokenType::TLessThan) ||
      CheckTokenType(TokenType::TGreaterThanEq) ||
      CheckTokenType(TokenType::TLessThanEq) ||
      CheckTokenType(TokenType::TGreaterThan) ||
      CheckTokenType(TokenType::TCompareEq) ||
      CheckTokenType(TokenType::TNotEq)) {
    // < or >= or ... or !=
    if (!Term())
      return false;

    // < or >= or ... or != <term>
    return RelationPrime();
  }

  // ε
  return true;
}

//////////////////////////// End Relation //////////////////////////////////

//////////////////////////////// Term //////////////////////////////////////

// <term> ::= <factor> <termPrime>
bool Parser::Term() {
  if (!Factor())
    return false;

  // <factor>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!TermPrime()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression term. Expected valid syntax");
    return false;
  }

  // <factor> <termPrime>
  return true;
}

// <termPrime> ::= * <factor> <termPrime> | / <factor> <termPrime> | ε
bool Parser::TermPrime() {
  if (CheckTokenType(TokenType::TMultiply) ||
      CheckTokenType(TokenType::TDivide)) {
    // * or /
    if (!Factor())
      return false;

    // * or / <factor>
    return TermPrime();
  }

  // ε
  return true;
}

////////////////////////////// End Term ////////////////////////////////////

// <factor> ::= ( <expression> ) | [ - ] <name> | [ - ] <number> | <string> | <char> | true | false
bool Parser::Factor() {
  bool minus = false;
  if (CheckTokenType(TokenType::TMinus))
    minus = true;

  if (Name()) {
    // Do some processing here.
    return true;
  }

  if (Number()) {
    // Do some processing here.
    return true;
  }

  if (minus) {
    QueueExpectedTokenError("Unexpected '-' before factor that is not a number or name");
    return false;
  }

  if (CheckTokenType(TokenType::TLeftParen)) {
    // (
    if (!Expression()) {
      QueueExpectedTokenError("Expected after expression after '(' in factor");
      return false;
    }

    // ( <expression>
    if (!CheckTokenType(TokenType::TRightParen)) {
      QueueExpectedTokenError("Expected ')' after expression in factor");
      return false;
    }

    // ( <expression> )
    return true;
  } else if (CheckTokenType(TokenType::TTrue) ||
             CheckTokenType(TokenType::TFalse)) { // Boolean types.
    return true;
  } else if (String()) { // String type.
    return true;
  } else if (Char()) { // Char type.
    return true;
  }

  return false;
}

// <loop_statement> ::= for ( <assignment_statement> ; <expression> ) ( <statement> ; )* end for
bool Parser::LoopStatement() {
  // <loop_statement> is not required; cannot queue error if first token is not
  // found.
  if (!CheckTokenType(TokenType::TFor))
    return false;

  // for
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' after 'for' in for loop");
    return false;
  }

  // for (
  int errorQueueSizeSnapshot = errorQueue_.size();
  // Only queue an error if AssignmentStatement did not (sometimes it does!).
  if (!AssignmentStatement() && errorQueueSizeSnapshot == errorQueue_.size()) {
    QueueExpectedTokenError("Expected assignment statement after '(' in for loop");
    return false;
  }

  // for ( <assignment_statement>
  if (!CheckTokenType(TokenType::TSemicolon)) {
    QueueExpectedTokenError("Expected ';' after assignment statement in for loop");
    return false;
  }

  // for ( <assignment_statement> ;
  if (!Expression()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected expression after ';' in for loop");
    return false;
  }

  // for ( <assignment_statement> ; <expression>
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after expression in for loop statement");
    return false;
  }

  // for ( <assignment_statement> ; <expression> )
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement in for loop");
      return false;
    }
  }

  // Same error checking as above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // for ( <assignment_statement> ; <expression> ) ( <statement> ; )*
  if (!CheckTokenType(TokenType::TEnd) || !CheckTokenType(TokenType::TFor)) {
    QueueExpectedTokenError("Expected 'end for' after for loop statement");
    return false;
  }

  // for ( <assignment_statement> ; <expression> ) ( <statement> ; )* end for
  return true;
}

// <if_statement> ::= if ( <expression> ) then ( <statement> ; )+ [ else ( <statement> ; )+ ] end if
bool Parser::IfStatement() {
  // <if_statement> is not required; cannot queue error if first token is not
  // found.
  if (!CheckTokenType(TokenType::TIf))
    return false;

  // if
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' after 'if' in if statement");
    return false;
  }

  // if (
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!Expression()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected expression in if statement after '('");
    return false;
  }

  // if ( <expression>
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after 'if' in if statement");
    return false;
  }

  // if ( <expression> )
  if (!CheckTokenType(TokenType::TThen)) {
    QueueExpectedTokenError("Expected 'then' after '( <expression> )' in if statement");
    return false;
  }

  // if ( <expression> ) then
  if (!Statement()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected at least one statement after 'then' in if statement");
    return false;
  }

  // if ( <expression> ) then <statement>
  if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement under 'then' in if statement");
    return false;
  }

  // if ( <expression> ) then <statement> ;
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement under 'then' in if statement");
      return false;
    }
  }

  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // if ( <expression> ) then ( <statement> ; )+
  if (CheckTokenType(TokenType::TElse)) {

    if (!Statement()) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected at least one statement after 'else' in if statement");
      return false;
    }

    // if ( <expression> ) then ( <statement> ; )+ else <statement>
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement under 'else' in if statement");
      return false;
    }

    // if ( <expression> ) then ( <statement> ; )+ else <statement> ;
    while (Statement()) {
      if (!CheckTokenType(TokenType::TSemicolon)) {
        QueueExpectedTokenError("Expected ';' after statement under 'else' in if statement");
        return false;
      }
    }

    if (errorQueue_.size() > errorQueueSizeSnapshot)
      return false;
  }

  // if ( <expression> ) then ( <statement> ; )+ [ else ( <statement> ; )+ ]
  if (!CheckTokenType(TokenType::TEnd) || !CheckTokenType(TokenType::TIf)) {
    QueueExpectedTokenError("Expected 'end if' after if statement");
    return false;
  }

  // if ( <expression> ) then ( <statement> ; )+ [ else ( <statement> ; )+ ] end if
  return true;
}

// <procedure_declaration> ::= <procedure_header> <procedure_body>
bool Parser::ProcedureDeclaration() {
  // This is not a valid ProcedureDeclaration, if the ProcedureHeader is either
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
  // eventually a (<declaration>;)*, which can appear no times), so if the first
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
  int errorQueueSizeSnapshot = errorQueue_.size();
  while (Declaration()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after declaration");
      return false;
    }
  }

  // Declaration() isn't required, so there are two reasons it may return false:
  // For these reasons, see ProgramBody() above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (!CheckTokenType(TokenType::TBegin)) {
    QueueExpectedTokenError("Expected 'begin' in procedure body");
    return false;
  }

  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement");
      return false;
    }
  }

  // Same error checking as above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (!CheckTokenType(TokenType::TEnd) ||
      !CheckTokenType(TokenType::TProcedure)) {
    QueueExpectedTokenError("Expected 'end program' after procedure body");
    return false;
  }

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
