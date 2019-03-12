#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "SymbolRecord.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <unordered_map>

class SymbolTable {
private:
  // TODO(domfarolino): If we want to support things like function overloading
  // we may need to utilize something like a multimap(?).
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

  // Function useful for debugging.
  void printTable() {
    std::cout << "+--------Symbol table--------+" << std::endl;
    for (auto it: table_) {
      // Don't print the built-ins.
      if (it.second.built_in) continue;
      std::cout << std::setw(10);
      if (it.second.type == SymbolType::Integer)
        std::cout << "integer ";
      else if (it.second.type == SymbolType::String)
        std::cout << "string ";
      else if (it.second.type == SymbolType::Float)
        std::cout << "float ";
      else if (it.second.type == SymbolType::Bool)
        std::cout << "bool ";
      else if (it.second.type == SymbolType::Char)
        std::cout << "char ";
      else if (it.second.type == SymbolType::Procedure)
        std::cout << "procedure ";

      std::cout << std::setw(15) << it.first;
      std::string boundString;
      if (it.second.isArray)
        boundString = "[" + it.second.lowerBound + " : " + it.second.upperBound + "]";
      std::cout << std::setw(9) << boundString;

      std::cout << " isGlobal: " << std::setw(7);
      if (it.second.isGlobal)
        std::cout << "global ";
      else
        std::cout << "-- ";

      std::cout << "paramType: " << std::setw(5);
      if (it.second.paramType == ParameterType::None)
        std::cout << "None";
      else if (it.second.paramType == ParameterType::In)
        std::cout << "In";
      else if (it.second.paramType == ParameterType::Out)
        std::cout << "Out";
      else if (it.second.paramType == ParameterType::InOut)
        std::cout << "InOut";

      std::cout << std::endl;
    }

    std::cout << "+--------End symbol table----+" << std::endl << std::endl;
  }
};

#endif
