#ifndef SYMBOL_RECORD_H
#define SYMBOL_RECORD_H

#include <string>
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

  SymbolRecord(): isArray(false), isGlobal(false), paramType(ParameterType::None) {}
  // TODO(domfarolino): Consider getting rid of this constructor.
  SymbolRecord(SymbolType inType, ParameterType inParamType): type(inType),
                                                              isArray(false),
                                                              isGlobal(false),
                                                              paramType(inParamType) {
    // A few checks to protect against using SymbolRecord incorrectly in some
    // of the low-hanging-fruit cases.
    if (type != SymbolType::Procedure && params.size() > 0)
      throw std::logic_error("Cannot construct a non-procedure symbol with parameters");

    if (type == SymbolType::Procedure && paramType != ParameterType::None)
      throw std::logic_error("Parameters of type procedure are not supported at this time");
  }

  SymbolType type;
  bool isArray;
  std::string lowerBound, upperBound; // Only relevant for arrays; 0 otherwise.
  bool isGlobal; // Only relevant in the outer-most global scope.
  // TODO(domfarolino): Maybe rename these?
  ParameterType paramType;
  std::vector<std::pair<std::string, SymbolRecord>> params;

  // TODO(domfarolino): Consider making Parser::TypeMark just return a
  // SymbolType, instead of abstracting conversion to here. The separation of
  // concerns is nice with this, but I don't like that if a grammar change is
  // made to <type_mark>, at least two changes in the impl are required.
  static SymbolType TypeMarkToSymbolType(const std::string& typeMark) {
    if (typeMark == "integer")
      return SymbolType::Integer;
    else if (typeMark == "string")
      return SymbolType::String;
    else if (typeMark == "float")
      return SymbolType::Float;
    else if (typeMark == "bool")
      return SymbolType::Bool;
    else if (typeMark == "char")
      return SymbolType::Char;
    else
      throw std::logic_error("SymbolRecord::TypeMarkToSymbolType was given an invalid type mark: " + typeMark);
  }

  static ParameterType ParameterTypeStringToParameterType(const std::string& parameterTypeString) {
    if (parameterTypeString == "in")
      return ParameterType::In;
    else if (parameterTypeString == "out")
      return ParameterType::Out;
    else if (parameterTypeString == "inout")
      return ParameterType::InOut;
    else
      throw std::logic_error("SymbolRecord::ParameterTypeStringToParameterType was given an invalid parameter type string: " + parameterTypeString);
  }
};

#endif
