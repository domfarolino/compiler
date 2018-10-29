#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "SymbolRecord.h"

#include <string>
#include <unordered_map>

class SymbolTable {
private:
  std::unordered_map<std::string, SymbolRecord> table_;

public:
  SymbolTable() {}

  bool lookup(const std::string& symbolName) {
    return !(table_.find(symbolName) == table_.end());
  }

  bool insert(const std::string& symbolName, SymbolRecord symbolRecord) {
    if (lookup(symbolName))
      return false;

    table_.insert({symbolName, symbolRecord});
    return true;
  }

  SymbolRecord* getSymbol(const std::string& symbolName) {
    if (!lookup(symbolName))
      return nullptr;

    return &table_.find(symbolName)->second;
  }
};

#endif
