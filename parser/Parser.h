#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <queue>
#include <utility>

#include "../lib/Token.h"

class Lexer;
class ScopeManager;
class SymbolRecord;

class Parser {
public:
  Parser(Lexer&, ScopeManager&, bool);

private:
  // Members
  Lexer& lexer_;
  ScopeManager& scopeManager_;
  Token token_;
  bool symbolInsight_;
  std::queue<std::string> errorQueue_;

  // Utility Methods
  bool CheckTokenType(TokenType);
  void QueueError(std::string);
  void QueueExpectedTokenError(std::string);
  void QueueSymbolError(std::string);
  void FlushErrors();

  // Productions
  void Program();
  bool ProgramHeader();
  bool ProgramBody();
  bool Identifier();
  bool Identifier(std::string&);
  bool String();
  bool Char();
  bool Name();
  bool Declaration();
  bool Statement();
  bool AssignmentStatement(std::string&, bool&);
  bool Destination(std::string&, SymbolRecord&, bool&);
  bool Expression();
  bool ExpressionPrime();
  bool ArithOp();
  bool ArithOpPrime();
  bool Relation();
  bool RelationPrime();
  bool Term();
  bool TermPrime();
  bool Factor();
  bool LoopStatement();
  bool IfStatement();
  bool ProcedureCall(std::string&);
  bool ArgumentList(std::vector<std::pair<std::string, SymbolRecord>>&);
  bool ProcedureDeclaration(std::string&, SymbolRecord&);
  bool ProcedureHeader(std::string&, std::vector<std::pair<std::string, SymbolRecord>>&);
  bool ParameterList(std::vector<std::pair<std::string, SymbolRecord>>&);
  bool Parameter(std::vector<std::pair<std::string, SymbolRecord>>&);
  bool ProcedureBody();
  bool VariableDeclaration(std::string&, SymbolRecord&);
  bool TypeMark(std::string&);
  bool LowerOrUpperBound(std::string&);
  bool Number(std::string&);
};

#endif
