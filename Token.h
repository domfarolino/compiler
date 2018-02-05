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
  TSemiColon,
  TComma,
  TPeriod,
  TLeftBracket,
  TRightBracket,
  TLeftParen,
  TRightParen,
  TEOF
};

std::unordered_map<std::string, TokenType> reservedWords;

class Token {
public:
  Token() {}
  Token(TokenType inType, const std::string& inLexeme): type(inType), lexeme(inLexeme) {}
  std::string lexeme;
  TokenType type;
};

#endif
