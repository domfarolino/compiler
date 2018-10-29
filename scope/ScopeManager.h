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

  bool insert(const std::string& symbolName, SymbolRecord symbolRecord) {
    return scopeStack.top().insert(symbolName, symbolRecord);
  }

  SymbolRecord* getSymbol(const std::string& symbolName) {
    if (scopeStack.top().lookup(symbolName))
      return scopeStack.top().getSymbol(symbolName);

    SymbolRecord* globalRecord = globalScope->getSymbol(symbolName);
    return (globalRecord && globalRecord->isGlobal) ? globalRecord : nullptr;
  }
};

#endif
