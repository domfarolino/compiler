#ifndef SYMBOL_RECORD_H
#define SYMBOL_RECORD_H

#include <string>
#include <stdexcept>
#include <vector>

namespace llvm {
  class Value;
}

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
    isArray = other.isArray;
    lowerBound = other.lowerBound;
    upperBound = other.upperBound;
    isGlobal = other.isGlobal;
    is_literal = other.is_literal;
    built_in = other.built_in;
    paramType = other.paramType;
    params = other.params;
    value = other.value;
  }
*/

  SymbolRecord(): isArray(false), isGlobal(false), is_literal(false),
                  built_in(false), paramType(ParameterType::None) {}
  // TODO(domfarolino): Consider getting rid of this constructor.
  SymbolRecord(SymbolType inType, ParameterType inParamType): type(inType),
                                                              isArray(false),
                                                              isGlobal(false),
                                                              is_literal(false),
                                                              built_in(false),
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
  bool is_literal;
  bool built_in;
  // TODO(domfarolino): Maybe rename these?
  ParameterType paramType;
  std::vector<std::pair<std::string, SymbolRecord>> params;
  llvm::Value* value;

  int array_length() {
    int return_length = std::stoi(upperBound) - std::stoi(lowerBound) + 1;
    // Assert: return_length >= 0.
    return return_length;
  }

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
    else if (typeMark == "procedure")
      throw std::logic_error("SymbolRecord::TypeMarkToSymbolType 'procedure' is not a valid <type_mark>");
    else
      throw std::logic_error("SymbolRecord::TypeMarkToSymbolType was given an invalid type mark: " +
                             typeMark);
  }

  static std::string SymbolTypeToDebugString(const SymbolType& symbolType) {
    if (symbolType == SymbolType::Integer)
      return "integer";
    else if (symbolType == SymbolType::String)
      return "string";
    else if (symbolType == SymbolType::Float)
      return "float";
    else if (symbolType == SymbolType::Bool)
      return "bool";
    else if (symbolType == SymbolType::Char)
      return "char";
    else if (symbolType == SymbolType::Procedure)
      return "procedure";
    else
      throw std::logic_error("SymbolRecord::SymbolTypeToDebugString was given an invalid SymbolType enum value: " +
                             std::to_string(symbolType));
  }

  static ParameterType ParameterTypeStringToParameterType(const std::string& parameterTypeString) {
    if (parameterTypeString == "in")
      return ParameterType::In;
    else if (parameterTypeString == "out")
      return ParameterType::Out;
    else if (parameterTypeString == "inout")
      return ParameterType::InOut;
    else
      throw std::logic_error("SymbolRecord::ParameterTypeStringToParameterType was given an invalid parameter type string: " +
                             parameterTypeString);
  }
};

#endif
