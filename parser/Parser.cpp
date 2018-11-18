#include "Parser.h"

#include <iostream>
#include <string>
#include <queue>

#include "../scope/ScopeManager.h"
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

Parser::Parser(Lexer& inLexer, ScopeManager &inScopes, bool inSymbolInsight):
                                              lexer_(inLexer),
                                              scopeManager_(inScopes),
                                              symbolInsight_(inSymbolInsight) {
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

// Queues a regular error, but stops the code generator from generating code.
void Parser::QueueSymbolError(std::string inErrorString) {
  // TODO(domfarolino): Tell the code generator to stop generating code.
  QueueError(inErrorString);
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
  // The ScopeManager is initialized with a global scope, so as long as there
  // is only one "program" in any given parse, we don't have to explicitly enter
  // a new scope.
  bool parsedHeader = ProgramHeader();
  if (!parsedHeader)
    QueueError("Error parsing program header");

  bool parsedBody = ProgramBody();
  if (!parsedBody) {
    QueueError("Error parsing program body");
    return;
  }

  if (symbolInsight_) scopeManager_.printTopScope();

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
  return CheckTokenType(TokenType::TIdentifier);
}

// <identifier> ::= [a-zA-Z][a-zA-Z0-9_]*
bool Parser::Identifier(std::string& id) {
  // TODO(domfarolino): Maybe if the token was not an identifier, we can check
  // to see if it is a reserved word and throw a more specific error, like "can
  // not use reserved word as identifier" or something.
  id = token_.lexeme;
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
bool Parser::Name() {
  // <name> is not required (see <factor>).
  if (!Identifier())
    return false;

  // <identifier>
  if (CheckTokenType(TokenType::TLeftBracket)) {

    int errorQueueSizeSnapshot = errorQueue_.size();
    // <identifier> [
    if (!Expression()) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
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
  std::string identifier;
  SymbolRecord symbolRecord;

  bool global = false;
  if (CheckTokenType(TokenType::TGlobal))
    global = true;

  symbolRecord.isGlobal = global;

  int errorQueueSizeSnapshot = errorQueue_.size();
  if (ProcedureDeclaration(identifier, symbolRecord))
    return true;
  else if (errorQueue_.size() == errorQueueSizeSnapshot &&
           VariableDeclaration(identifier, symbolRecord)) {

    // The caller of VariableDeclaration is responsible for inserting its
    // symbol into the current scope, hence why we have this conditional.
    if (!scopeManager_.insert(identifier, symbolRecord)) {
      QueueSymbolError("Symbol '" + identifier + "' already exists, cannot redeclare");
      return false;
    }

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
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (IfStatement()) return true;
  else if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (LoopStatement()) return true;
  else if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  std::string identifier;
  bool validIdentifier = true;
  if (AssignmentStatement(identifier, validIdentifier)) return true;
  else if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // Assert: There were no parsing errors specific to AssignmentStatement. In
  // other words, AssignmentStatement failed without queueing an error, and this
  // only happens when:
  //  1.) The next token was not a valid identifier, therefore
  //      AssignmentStatment and ProcedureCall will both return false with no
  //      queued error (because these productions are not mandatory)
  //  2.) The next token was a valid identifier, but the symbol associated with
  //      it was type procedure, and therefore ProcedureCall must handle it
  if (validIdentifier && ProcedureCall(identifier)) return true;

  return false;
}

// <assignment_statement> ::= <destination> := <expression>
bool Parser::AssignmentStatement(std::string& identifier, bool& validIdentifier) {
  SymbolRecord symbolRecord;
  // <assignment_statement> is not required, as up the chain it is part of a
  // (<statement>;)*, so if we fail to find the first token we cannot queue
  // an error quite yet.
  if (!Destination(identifier, symbolRecord, validIdentifier))
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
bool Parser::Destination(std::string& identifier, SymbolRecord& symbolRecord, bool& validIdentifier) {
  // <destination> is not required (see <assignment_statement>).
  if (!Identifier(identifier)) {
    validIdentifier = false;
    return false;
  }

  if (!scopeManager_.lookup(identifier)) {
    QueueSymbolError("Symbol '" + identifier + "' not found");
    return false;
  }

  // Procedures cannot be destinations. If the author was trying to assign or
  // index into a procedure, then ProcedureCall will catch that.
  if (scopeManager_.getSymbol(identifier)->type == SymbolType::Procedure)
    return false;

  // <identifier>
  if (CheckTokenType(TokenType::TLeftBracket)) {

    int errorQueueSizeSnapshot = errorQueue_.size();
    // <identifier> [
    if (!Expression()) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
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

  std::string number;
  if (Number(number)) {
    // Do some processing here.
    return true;
  }

  if (minus) {
    QueueExpectedTokenError("Unexpected '-' before factor that is not a number or name");
    return false;
  }

  if (CheckTokenType(TokenType::TLeftParen)) {
    // (
    int errorQueueSizeSnapshot = errorQueue_.size();
    if (!Expression()) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected expression after '(' in factor");
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
  std::string identifier;
  bool validIdentifier = true;
  // Only queue an error if AssignmentStatement did not (sometimes it does!).
  if (!AssignmentStatement(identifier, validIdentifier) &&
      errorQueueSizeSnapshot == errorQueue_.size()) {
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

// <procedure_call> ::= <identifier> ( [<argument_list>] )
bool Parser::ProcedureCall(std::string& identifier) {
  // ProcedureCall is not required, so don't queue error if first token is not
  // found.
  // We are given a valid identifier. If a valid identifier could not be found
  // then Statement will know this, and report it without calling us.
  //if (!Identifier())
    //return false;

  // <identifier>
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' after procedure identifier to invoke procedure");
    return false;
  }

  // <identifier> (
  // An ArgumentList is optional for procedure invocation, so if
  // ArgumentList() returned false only because it didn't exist, we can't let
  // that make us return false here, since the rest of ProcedureCall existed
  // just fine. We only want to propagate errors.
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ArgumentList() && errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // <identifier> (...
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after argument list");
    return false;
  }

  // <identifier> (...)
  return true;
}

// <argument_list> ::= <expression> , <argument_list> | <expression>
bool Parser::ArgumentList() {
  // The language allows empty argument lists, so Expression() could have
  // returned false because it didn't exist, or because it failed to parse.
  // If it failed to parse, Expression will take care of queueing an error.
  if (!Expression())
    return false;

  int errorQueueSizeSnapshot = errorQueue_.size();
  while (CheckTokenType(TokenType::TComma)) {
    if (!Expression()) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueError("Expected argument after ',' in argument list");
      return false;
    }
  }

  return true;
}

// <procedure_declaration> ::= <procedure_header> <procedure_body>
bool Parser::ProcedureDeclaration(std::string& identifier, SymbolRecord& symbolRecord) {
  // TODO(domfarolino): Consider making this function take in a global flag
  // instead of a symbol record, so we can create and add the symbol record here.

  symbolRecord.type = SymbolType::Procedure;
  SymbolTable& outerScope = scopeManager_.getCurrentScopeRef();
  std::vector<std::pair<std::string, SymbolRecord>> parameters;
  // This is not a valid ProcedureDeclaration, if the ProcedureHeader is either
  // missing or invalid.
  if (!ProcedureHeader(identifier, parameters))
    return false;

  // Create the final procedure symbol record.
  symbolRecord.params = parameters;

  // Enter procedure scope.
  scopeManager_.enterScope();

  // Add the procedure's symbol to its own symbol table.
  if (!scopeManager_.insertAllowShadow(identifier, symbolRecord))
    throw std::logic_error("Unreachable: Cannot insert procedure into its own local scope");

  // Add each parameter's symbol to the current procedure's symbol table.
  for (auto parameterInfo: parameters) {
    // parameterInfo.first = identifier.
    // parameterInfo.second = symbol record.

    if (!scopeManager_.insertAllowShadow(parameterInfo.first, parameterInfo.second)) {
      QueueSymbolError("Cannot add parameter '" + parameterInfo.first +
                       "' to the current scope, symbol name already exists");
      return false;
    }
  }

  // Insert the symbol into the "outer" scope that it appears within.
  if (!scopeManager_.insertAtScope(identifier, symbolRecord, outerScope)) {
    QueueSymbolError("Symbol '" + identifier + "' already exists, cannot redeclare");
    return false;
  }

  // <procedure_header>
  // Same goes for ProcedureBody...
  bool parsedBody = ProcedureBody();
  
  if (symbolInsight_) scopeManager_.printTopScope();

  scopeManager_.leaveScope();
  return parsedBody;
}

// <procedure_header> :: = procedure <identifier> ( [<parameter_list>] )
bool Parser::ProcedureHeader(std::string& identifier, std::vector<std::pair<std::string, SymbolRecord>>& parameters) {
  // A <procedure_header> doesn't always have to exist (as up the chain it is
  // eventually a (<declaration>;)*, which can appear no times), so if the first
  // terminal of a production like this, we have to let our caller decide
  // whether it is appropriate to throw an error. This is very similar to what
  // we do in VariableDeclaration, since it seeks for the first terminal in a
  // production that doesn't always have to produce anything.
  if (!CheckTokenType(TokenType::TProcedure))
    return false;

  // procedure
  if (!Identifier(identifier)) {
    QueueExpectedTokenError("Expected identifier in procedure header");
    return false;
  }

  // procedure <identifier>
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' before parameter list in procedure header");
    return false;
  }

  // procedure <identifier> (
  // A ParameterList is optional, so if ParameterList() returned false only
  // because it didn't exist, we can't let that make us return false here, since
  // the rest of ProcedureHeader existed just fine. We only want to propagate
  // errors.
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ParameterList(parameters) && errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // procedure <identifier> (..."
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after parameter list in procedure header");
    return false;
  }

  // procedure <identifier> (...)
  return true;
}

// <parameter_list> ::= <parameter> , <parameter_list> | <parameter>
bool Parser::ParameterList(std::vector<std::pair<std::string, SymbolRecord>>& parameters) {
  // The language allows empty parameter lists, so Parameter() could have
  // returned false because it didn't exist, or because it failed to parse.
  // Either way, we return false, and our caller will detect whether there
  // was an error or not. If Parameter() was true, we only return true if
  // there are no trailing commas in the list.
  if (!Parameter(parameters))
    return false;

  while (CheckTokenType(TokenType::TComma)) {
    if (!Parameter(parameters)) {
      QueueError("Expected parameter after ',' in parameter list");
      return false;
    }
  }

  return true;
}

// <parameter> ::= <variable_declaration> (in | out | inout)
bool Parser::Parameter(std::vector<std::pair<std::string, SymbolRecord>>& parameters) {
  // If VD returns false, that could be because it didn't find a VD, or because
  // a VD failed to parse. Parameters aren't mandatory, so we only care about
  // the latter case. Our caller will determine whether the VD had an error
  // parsing or not. In this frame, we only care about completion.
  std::string identifier;
  SymbolRecord symbolRecord;
  if (!VariableDeclaration(identifier, symbolRecord))
    return false;

  if (!CheckTokenType(TokenType::TIn) &&
      !CheckTokenType(TokenType::TOut) &&
      !CheckTokenType(TokenType::TInOut)) {
    QueueExpectedTokenError("Expected 'in', 'out', or 'inout' after parameter identifier");
    return false;
  }

  // TODO(domfarolino): Set parameter's symbol record's type accordingly.
  parameters.push_back(std::make_pair(identifier, symbolRecord));
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

  // Declaration() isn't required, so there are two reasons it may return false.
  // For these reasons, see ProgramBody() above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // Assert: We ran out of Declarations, so really if we see anything that is
  // not `begin`, we were expecting `begin` or more declarations.
  // (<declaration>;)*
  if (!CheckTokenType(TokenType::TBegin)) {
    QueueExpectedTokenError("Expected 'begin' or declaration in procedure body");
    return false;
  }

  // (<declaration>;)* begin
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement");
      return false;
    }
  }

  // Same error checking as above (like w/ Declaration).
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // (<declaration>;)* begin (<statement>;)
  if (!CheckTokenType(TokenType::TEnd) ||
      !CheckTokenType(TokenType::TProcedure)) {
    QueueExpectedTokenError("Expected 'end program' after procedure body");
    return false;
  }

  // (<declaration>;)* begin (<statement>;)* end procedure
  return true;
}

// <variable_declaration> ::= <type_mark> <identifier> [ [ <lower_bound> “:” <upper_bound> ] ]
bool Parser::VariableDeclaration(std::string& identifier, SymbolRecord& symbolRecord) {
  // Can't report error if TypeMark was not found, because VariableDeclaration
  // isn't always required. Caller will decide whether a VD that doesn't exist
  // is an error or not, given the context.
  std::string typeMark;
  if (!TypeMark(typeMark))
    return false;

  symbolRecord.type = SymbolRecord::TypeMarkToSymbolType(typeMark);

  // <type_mark>
  if (!Identifier(identifier)) {
    QueueExpectedTokenError("Expected identifier in variable declaration");
    return false;
  }

  // <type_mark> <identifier>
  if (CheckTokenType(TokenType::TLeftBracket)) {
    symbolRecord.isArray = true;

    // Declaring an array, lower and upper bound are required.
    // <type_mark> <identifier> [
    if (!LowerOrUpperBound(symbolRecord.lowerBound)) {
      QueueExpectedTokenError("Expected number for array lower bound");
      return false;
    }

    // <type_mark> <identifier> [ <number>
    if (!CheckTokenType(TokenType::TColon)) {
      QueueExpectedTokenError("Expected ':' separating lower and upper array bounds");
      return false;
    }

    // <type_mark> <identifier> [ <number> :
    if (!LowerOrUpperBound(symbolRecord.upperBound)) {
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
bool Parser::TypeMark(std::string& typeMark) {
  typeMark = token_.lexeme;
  return CheckTokenType(TokenType::TIntegerType) ||
         CheckTokenType(TokenType::TFloatType)   ||
         CheckTokenType(TokenType::TStringType)  ||
         CheckTokenType(TokenType::TBoolType)    ||
         CheckTokenType(TokenType::TCharType);
}

// <lower_bound> ::= [-] <number>
// <upper_bound> ::= [-] <number>
bool Parser::LowerOrUpperBound(std::string& number) {
  if (CheckTokenType(TokenType::TMinus))
    number += '-';

  return Number(number);
}

// <number> ::= [0-9][0-9_]*[.[0-9_]*]
// Not necessary to find one in all usages, so we leave error reporting to the
// caller.
bool Parser::Number(std::string& number) {
  number += token_.lexeme;
  return CheckTokenType(TokenType::TInteger) ||
         CheckTokenType(TokenType::TFloat);
}
