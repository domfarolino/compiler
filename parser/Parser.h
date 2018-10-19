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
  void QueueExpectedTokenError(std::string);
  void FlushErrors();

  // Productions
  void Program();
  bool ProgramHeader();
  bool ProgramBody();
  bool Identifier();
  bool String();
  bool Char();
  bool Name();
  bool Declaration();
  bool Statement();
  bool AssignmentStatement();
  bool Destination();
  bool Expression();
  bool ExpressionPrime();
  bool ArithOp();
  bool ArithOpPrime();
  bool Relation();
  bool RelationPrime();
  bool Term();
  bool TermPrime();
  bool Factor();
  bool ProcedureDeclaration();
  bool ProcedureHeader();
  bool ParameterList();
  bool Parameter();
  bool ProcedureBody();
  bool VariableDeclaration();
  bool TypeMark();
  bool LowerOrUpperBound();
  bool Number();
};

#endif
