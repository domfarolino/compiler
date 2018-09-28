#include "Lexer.h"

#include <iostream>
#include <fstream>
#include <regex>
#include <string>

#include "../lib/Token.h"

Lexer::Lexer(const std::string& program, bool inVerbose = false): source(program, std::fstream::in), lineNumber(1), done(false), verbose_(inVerbose),
  reservedWords({
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
    {"integer", TokenType::TIntegerType},
    {"float", TokenType::TFloatType},
    {"string", TokenType::TStringType},
    {"bool", TokenType::TBoolType},
    {"char", TokenType::TCharType},
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
    {":", TokenType::TColon},
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
  }) {
  if (!source.is_open()) {
    std::cerr << "Failed to open " << program << std::endl;
    done = true;
  }
}

Lexer::~Lexer() {
  source.close();
}

/**
 * TODO: Allow us to deal with the lexing of both comments,
 * and nested comments (some extra work will need to be done here,
 * likely in and outside of the getNextToken function).
 * See https://github.com/domfarolino/compiler/issues/3
 */
Token Lexer::nextToken() {
  // Handle whitespace
  processWhitespace();

  // Just peek at the first character
  char peek;
  peek = source.get();
  source.putback(peek);

  Token returnToken;

  if (isAlphabetCharacter(peek)) {
    // Token is an identifier or reserved word
    returnToken = processIdentifierOrReservedWord();
  } else if (isDoubleQuote(peek)) {
    // Token is a string terminal
    returnToken = processString();
  } else if (isSingleQuote(peek)) {
    // Token is a character terminal
    returnToken = processCharacter();
  } else if (isDigit(peek)) {
    // Token is an integer or float terminal
    returnToken = processIntegerOrFloat();
  } else if (isSpecialCharacter(peek)) {
    returnToken = processSpecialCharacter();
  } else if (peek == EOF) {
    returnToken = Token('\0', TokenType::TEOF);
    done = true;
    if (verbose_) std::cout << "Lexing complete..." << std::endl;
  } else {
    returnToken = processInvalid();
  }

  if (returnToken.type != TokenType::TInvalid &&
      returnToken.type != TokenType::TEOF &&
      verbose_) {
    std::cout << returnToken.lexeme << std::endl;
  }

  return returnToken;
}

Token Lexer::processIdentifierOrReservedWord() {
  Token returnToken(source.get(), TokenType::TIdentifier);

  // Build lexeme
  while (std::regex_match(returnToken.lexeme, std::regex("[a-zA-Z][a-zA-Z0-9_]*"))) {
    returnToken.lexeme.push_back(source.get());
  }

  source.putback(returnToken.lexeme.back());
  returnToken.lexeme.pop_back();

  // Check for reserved word
  if (reservedWords.find(returnToken.lexeme) != reservedWords.end()) {
    if (verbose_) std::cout << "Tokenizing reserved word: ";
    returnToken.type = reservedWords[returnToken.lexeme];
  } else if (verbose_)
    std::cout << "Tokenizing identifier: ";

  return returnToken;
}

Token Lexer::processString() {
  Token returnToken(source.get(), TokenType::TString);
  if (verbose_) std::cout << "Tokenizing string: ";

  while (std::regex_match(returnToken.lexeme, std::regex("\"[a-zA-Z0-9 _,;:.']*"))) {
    returnToken.lexeme.push_back(source.get());
  }

  if (returnToken.lexeme[returnToken.lexeme.size() - 1] != '\"') {
    returnToken.type = TokenType::TInvalid;
    source.putback(returnToken.lexeme.back());
    displayLexerError(returnToken);
  }

  return returnToken;
}

Token Lexer::processCharacter() {
  Token returnToken(source.get(), TokenType::TChar);
  bool invalid = false;
  if (verbose_) std::cout << "Tokenizing char: ";

  // Get second character
  returnToken.lexeme.push_back(source.get());
  if (!std::regex_match(returnToken.lexeme, std::regex("'[a-zA-Z0-9 _;:.\"]"))) {
    invalid = true;
  }

  // Get third character
  if (!invalid) {
    returnToken.lexeme.push_back(source.get());
    if (!std::regex_match(returnToken.lexeme, std::regex("'[a-zA-Z0-9 _;:.\"]'"))) {
      invalid = true;
    }
  }

  if (invalid) {
    returnToken.type = TokenType::TInvalid;
    source.putback(returnToken.lexeme.back());
    displayLexerError(returnToken);
  }

  return returnToken;
}

Token Lexer::processIntegerOrFloat() {
  Token returnToken(source.get());
  char nextChar;

  while (std::regex_match(returnToken.lexeme, std::regex("[0-9][0-9_]*\\.?[0-9_]*"))) {
    nextChar = source.get();
    if (nextChar != '_') returnToken.lexeme.push_back(nextChar);
  }

  source.putback(nextChar);
  returnToken.lexeme.pop_back();

  // What kind of token is it?
  if (verbose_) {
    if (returnToken.lexeme.find('.') == std::string::npos) {
      std::cout << "Tokenizing TInteger: ";
    } else {
      std::cout << "Tokenizing TFloat: ";
    }
  }

  returnToken.type = (returnToken.lexeme.find('.') == std::string::npos) ? TokenType::TInteger : TokenType::TFloat;
  return returnToken;
}

Token Lexer::processSpecialCharacter() {
  Token returnToken(source.get());

  bool hasMatched = false;
  bool doesMatch = false;
  while (isSpecialCharacter(returnToken.lexeme.back())) {
    doesMatch = (reservedWords.find(returnToken.lexeme) != reservedWords.end());

    if (doesMatch)
      hasMatched = true;

    if (!doesMatch && hasMatched)
      break;

    returnToken.lexeme.push_back(source.get());
  }

  source.putback(returnToken.lexeme.back());
  returnToken.lexeme.pop_back();

  // Check for reserved word
  if (reservedWords.find(returnToken.lexeme) != reservedWords.end()) {
    if (verbose_) std::cout << "Tokenizing reserved word: ";
    returnToken.type = reservedWords[returnToken.lexeme];
  } else {
    // Only reserved words can start with special symbols in this
    // language, so tokens starting with special symbols all the way
    // up to the first non-special symbol are invalid unless they match
    // a reserved word
    returnToken.type = TokenType::TInvalid;
    displayLexerError(returnToken);
  }

  return returnToken;
}

Token Lexer::processInvalid() {
  Token returnToken(source.get(), TokenType::TInvalid);

  while (isInvalidCharacter(returnToken.lexeme.back())) {
    returnToken.lexeme.push_back(source.get());
  }

  source.putback(returnToken.lexeme.back());
  returnToken.lexeme.pop_back();
  displayLexerError(returnToken);
  return returnToken;
}

void Lexer::processWhitespace() {
  char currentChar = source.get();
  while (isWhitespace(currentChar)) {
    if (isNewline(currentChar)) lineNumber++;
    currentChar = source.get();
  }

  // We've now hit something other than whitespace,
  // put it back so our caller can use it.
  source.putback(currentChar);
}

void Lexer::displayLexerError(Token invalidToken) {
  // Only the default error message for now
  std::cerr << "\033[1;31mCould not tokenize: \033[0m" << invalidToken.lexeme << " \033[1;31mon line " << lineNumber << "\033[0m"<< std::endl;
}

void Lexer::displayLexerWarning(Token invalidToken) {
  // Not implemented yet, everything is an error for now
}
