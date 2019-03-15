#ifndef TOKEN_H
#define TOKEN_H

enum class TokenType {
  // Types
  TIntegerType,
  TInteger,
  TFloatType,
  TFloat,
  TStringType,
  TString,
  TBoolType, // TTrue, TFalse below
  TCharType,
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
  TInvalid,
  // Comments
  TShortComment,
  TBeginComment,
  TEndComment,
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
