#ifndef SCOPE_MANAGER_H
#define SCOPE_MANAGER_H

#include "SymbolTable.h"
#include "SymbolRecord.h"

#include <string>
#include <stack>
#include <stdexcept>

/**
 * The ScopeManager class is responsible for maintaining a stack of
 * SymbolTables, of which the top stack is the most local scope that
 * the parser is in at a given time, and the bottom-most stack is the
 * outer-most special "global" scope.
 */

class ScopeManager {
private:
  std::stack<SymbolTable> scopeStack;
  // TODO(domfarolino): Can we change this to a reference?
  SymbolTable* globalScope;

public:
  ScopeManager() {
    SymbolTable initScope;
    scopeStack.push(initScope);
    globalScope = &scopeStack.top();
  }

  void enterScope() { scopeStack.push(SymbolTable()); }

  void leaveScope() {
    if (scopeStack.size() == 1)
      throw std::logic_error("Attempting to leave the final global scope, which breaks Scope Manager's invariant");

    scopeStack.pop();
  }

  bool lookup(const std::string& symbolName) {
    SymbolRecord* globalRecord = globalScope->getSymbol(symbolName);
    if (scopeStack.top().lookup(symbolName) || (globalRecord && globalRecord->isGlobal))
      return true;

    return false;
  }

  bool lookupAtScope(const std::string& symbolName, SymbolTable& scope) {
    SymbolRecord* globalRecord = globalScope->getSymbol(symbolName);
    if (scope.lookup(symbolName)  || (globalRecord && globalRecord->isGlobal))
      return true;

    return false;
  }

  bool insert(const std::string& symbolName, SymbolRecord symbolRecord) {
    if (lookup(symbolName))
      return false;

    return scopeStack.top().insert(symbolName, symbolRecord);
  }

  bool insertAtScope(const std::string& symbolName, SymbolRecord symbolRecord, SymbolTable& scope) {
    if (lookupAtScope(symbolName, scope))
      return false;

    return scope.insert(symbolName, symbolRecord);
  }

  // This allows symbols to shadow symbols from outer scope(s), but does not
  // allow symbols within the same scope to shadow each other. At the time of
  // writing, this is only used when inserting a procedure's symbol and its
  // parameters into the local scope of said procedure.
  bool insertAllowShadow(const std::string& symbolName, SymbolRecord symbolRecord) {
    if (scopeStack.top().lookup(symbolName))
      return false;

    return scopeStack.top().insert(symbolName, symbolRecord);
  }

  // This returns a pointer only so that `nullptr` can act as a sentinel value
  // representing unfound symbols.
  SymbolRecord* getSymbol(const std::string& symbolName) {
    if (scopeStack.top().lookup(symbolName))
      return scopeStack.top().getSymbol(symbolName);

    SymbolRecord* globalRecord = globalScope->getSymbol(symbolName);
    return (globalRecord && globalRecord->isGlobal) ? globalRecord : nullptr;
  }

  SymbolTable& getCurrentScopeRef() {
    return scopeStack.top();
  }

  void printTopScope() {
    scopeStack.top().printTable();
  }
};

#endif
