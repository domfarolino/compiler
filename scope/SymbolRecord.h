#ifndef SYMBOL_RECORD_H
#define SYMBOL_RECORD_H

#include <stdexcept>
#include <vector>

enum SymbolType {
  Integer,
  String,
  Float,
  Bool,
  Char,
  Procedure
};

enum ParameterType {
  None,
  In,
  Out,
  InOut
};

class SymbolRecord {
public:
/*
  SymbolRecord(const SymbolRecord& other) {
    type = other.type;
    isGlobal = other.isGlobal;
    paramType = other.paramType;
    params = other.params;
  }
*/

  SymbolRecord(SymbolType inType, ParameterType inParamType,
               const std::vector<SymbolRecord>& inParams): type(inType),
                                                           isGlobal(false),
                                                           paramType(inParamType),
                                                           params(inParams) {
    // A few checks to protect against using SymbolRecord incorrectly in some
    // of the low-hanging-fruit cases.
    if (type != SymbolType::Procedure && params.size() > 0)
      throw std::logic_error("Cannot construct a non-procedure symbol with parameters");

    if (type == SymbolType::Procedure && paramType != ParameterType::None)
      throw std::logic_error("Parameters of type procedure are not supported at this time");
  }

  SymbolType type;
  bool isGlobal; // Only relevant in the outer-most global scope.
  ParameterType paramType;
  std::vector<SymbolRecord> params;
};

#endif
