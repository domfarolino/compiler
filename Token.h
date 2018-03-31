#ifndef TOKEN_H
#define TOKEN_H

#include <iostream>
#include <unordered_map>

enum class TokenType {
  // Types
  TInteger,
  TFloat,
  TString,
  TBool,
  TChar,
  TIdentifier,
  // Whitespace
  TSpace,
  TNewline,
  TTab,
  // Comments
  TCommentBegin,
  TCommentEnd,
  // Structure
  TProgram,
  TIs,
  TBegin,
  TProcedure,
  TIf,
  TThen,
  TElse,
  TFor,
  TEnd,
  // Operators
  TAmp,
  TOr,
  TNot,
  TPlus,
  TMinus,
  TLessThan,
  TLessThanEq,
  TGreaterThan,
  TGreaterThanEq,
  TCompareEq,
  TColonEq,
  TNotEq,
  TMultiply,
  TDivide,
  // Other reserved
  TGlobal,
  TIn,
  TOut,
  TInOut,
  TTrue,
  TFalse,
  TReturn,
  TColon,
  TSemicolon,
  TComma,
  TPeriod,
  TLeftBracket,
  TRightBracket,
  TLeftParen,
  TRightParen,
  TEOF,
  TInvalid
};

// All reserved terminals in the language's grammar
std::unordered_map<std::string, TokenType> reservedWords( {
  {".", TokenType::TPeriod},
  {"program", TokenType::TProgram},
  {"is", TokenType::TIs},
  {";", TokenType::TSemicolon},
  {"begin", TokenType::TBegin},
  {"end", TokenType::TEnd},
  {"global", TokenType::TGlobal},
  {"procedure", TokenType::TProcedure},
  {"(", TokenType::TLeftParen},
  {")", TokenType::TRightParen},
  {"in", TokenType::TIn},
  {"out", TokenType::TOut},
  {"inout", TokenType::TInOut},
  {"integer", TokenType::TInteger},
  {"float", TokenType::TFloat},
  {"string", TokenType::TString},
  {"bool", TokenType::TBool},
  {"char", TokenType::TChar},
  {"[", TokenType::TLeftBracket},
  {"]", TokenType::TRightBracket},
  {"if", TokenType::TIf},
  {"then", TokenType::TThen},
  {"else", TokenType::TElse},
  {"for", TokenType::TFor},
  {"return", TokenType::TReturn},
  {"&", TokenType::TAmp},
  {"|", TokenType::TOr},
  {"not", TokenType::TNot},
  {"+", TokenType::TPlus},
  {"-", TokenType::TMinus},
  {":=", TokenType::TColonEq},
  {"<", TokenType::TLessThan},
  {">=", TokenType::TGreaterThanEq},
  {"<=", TokenType::TLessThanEq},
  {">", TokenType::TGreaterThan},
  {"==", TokenType::TCompareEq},
  {"!=", TokenType::TNotEq},
  {"*", TokenType::TMultiply},
  {"/", TokenType::TDivide},
  {"true", TokenType::TTrue},
  {"false", TokenType::TFalse}
});

class Token {
public:
  Token() {}
  Token(TokenType inType, const std::string& inLexeme): type(inType), lexeme(inLexeme) {}
  std::string lexeme;
  TokenType type;
};

#endif
