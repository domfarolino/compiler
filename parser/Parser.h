#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <queue>
#include <utility>

#include "../lib/Token.h"
#include "../scope/SymbolRecord.h"

class Lexer;
class ScopeManager;

class Parser {
public:
  Parser(const std::string&, Lexer&, ScopeManager&, bool, bool);

private:
  // Members
  std::string program_name_;
  Lexer& lexer_;
  ScopeManager& scopeManager_;
  Token token_;
  bool symbolInsight_;
  bool code_gen_;
  std::queue<std::string> errorQueue_;

  // Utility Methods
  bool CheckTokenType(TokenType);
  void QueueError(std::string);
  void QueueExpectedTokenError(std::string);
  void QueueSymbolError(std::string);
  void QueueTypeError(std::string);
  void FlushErrors();
  void Setup();

  // Productions
  void Program();
  bool ProgramHeader();
  bool ProgramBody();
  bool Identifier();
  bool Identifier(std::string&);
  bool String(std::string&, SymbolRecord&);
  bool Char(std::string&, SymbolRecord&);
  bool Name(std::string&, SymbolRecord&);
  bool Declaration();
  bool Statement();
  bool AssignmentStatement(std::string&, bool&);
  bool Destination(std::string&, SymbolRecord&, bool&);
  bool Expression(SymbolRecord&);
  bool ExpressionPrime(SymbolRecord&);
  bool ArithOp(SymbolRecord&);
  bool ArithOpPrime(SymbolRecord&);
  bool Relation(SymbolRecord&);
  bool RelationPrime(SymbolRecord&);
  bool Term(SymbolRecord&);
  bool TermPrime(SymbolRecord&);
  bool Factor(SymbolRecord&);
  bool LoopStatement();
  bool IfStatement();
  bool ProcedureCall(std::string&);
  bool ReturnStatement();
  bool ArgumentList(std::vector<SymbolRecord>&);
  bool ProcedureDeclaration(std::string&, SymbolRecord&);
  bool ProcedureHeader(std::string&, std::vector<std::pair<std::string, SymbolRecord>>&);
  bool ParameterList(std::vector<std::pair<std::string, SymbolRecord>>&);
  bool Parameter(std::vector<std::pair<std::string, SymbolRecord>>&);
  bool ProcedureBody();
  bool VariableDeclaration(std::string&, SymbolRecord&);
  bool TypeMark(std::string&);
  bool LowerOrUpperBound(std::string&);
  bool Number(std::string&, SymbolRecord&);
};

#endif
