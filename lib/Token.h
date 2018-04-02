#ifndef TOKEN_H
#define TOKEN_H

#include <iostream>

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

class Token {
public:
  Token() {}
  Token(const std::string& inLexeme, TokenType inType = TokenType::TInvalid): lexeme(inLexeme), type(inType) {}
  Token(const char& inChar, TokenType inType = TokenType::TInvalid): lexeme(1, inChar), type(inType) {}
  std::string lexeme;
  TokenType type;
};

#endif