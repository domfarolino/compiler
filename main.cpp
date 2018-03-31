#include <iostream>
#include <fstream>
#include <regex>

#include "Token.h"

bool isUpper(char c) {
  return ('A' <= c && c <= 'Z');
}

bool isLower(char c) {
  return ('a' <= c && c <= 'z');
}

bool isSpecialChar(char c) {
  return (c == '.' || c == ';' ||
          c == '(' || c == ')' ||
          c == '[' || c == ']' ||
          c == '&' || c == '|' ||
          c == '+' || c == '-' ||
          c == ':' || c == '<' ||
          c == '>' || c == '=' ||
          c == '!' || c == '*' ||
          c == '/');
}

bool isDigit(char c) {
  return ('0' <= c && c <= '9');
}

bool isWhitespace(char c) {
  // TODO: handle other newline characters
  return (c == ' ' || c == '\n' || c == '\t');
}

bool isInvalidChar(char c) {
  return !isUpper(c) && !isLower(c) && !isSpecialChar(c) && !isDigit(c) && !isWhitespace(c);
}

/**
 * TODO: Allow us to deal with the lexing
 * of both comments, and nested comments
 * (some extra work will need to be done here,
 * likely in and outside of the getNextToken
 * function).
 */

Token getNextToken(std::ifstream& source) {
  char currentChar, nextChar;
  currentChar = source.get();

  // Handle whitespace
  while (isWhitespace(currentChar)) {
    currentChar = source.get();
  }

  /**
   * Token's starting character is either
   *   - Whitespace
   *   - Alphabet character
   *   - Number
   *   - Special symbol
   *   - EOF
   *   - Invalid character (does not appear in language specification)
   */

  Token returnToken;
  returnToken.lexeme += currentChar;

  if (isUpper(currentChar) || isLower(currentChar)) {
    // Token is an identifier until proven a reserved word
    returnToken.type = TokenType::TIdentifier;

    // Build lexeme
    while (std::regex_match(returnToken.lexeme, std::regex("[a-zA-Z][a-zA-Z0-9_]*"))) {
      currentChar = source.get();
      returnToken.lexeme += currentChar;
    }

    returnToken.lexeme.pop_back();
    source.putback(currentChar);

    // Check for reserved word
    if (reservedWords.find(returnToken.lexeme) != reservedWords.end()) {
      std::cout << "Tokenizing reserved word: ";
      returnToken.type = reservedWords[returnToken.lexeme];
    } else {
      std::cout << "Tokenizing identifier: ";
    }
  } else if (isDigit(currentChar)) {

    // Build lexeme
    while (std::regex_match(returnToken.lexeme, std::regex("[0-9][0-9_]*\\.?[0-9_]*"))) { // TODO: verify this regex (it is a modification of the spec)
      currentChar = source.get();
      if (currentChar != '_') returnToken.lexeme += currentChar;
    }

    returnToken.lexeme.pop_back();
    source.putback(currentChar);
    // Token is a number
    if (returnToken.lexeme.find('.') == std::string::npos) {
      std::cout << "Tokenizing TInteger: ";
    } else {
      std::cout << "Tokenizing TFloat: ";
    }
    returnToken.type = (returnToken.lexeme.find('.') == std::string::npos) ? TokenType::TInteger : TokenType::TFloat;
  } else if (isSpecialChar(currentChar)) {      
    // Token is a reserved word starting with a special character

    while (isSpecialChar(currentChar)) {
      currentChar = source.get();
      returnToken.lexeme += currentChar;
    }

    returnToken.lexeme.pop_back();
    source.putback(currentChar);

    // Check for reserved word
    if (reservedWords.find(returnToken.lexeme) != reservedWords.end()) {
      std::cout << "Tokenizing reserved word: ";
      returnToken.type = reservedWords[returnToken.lexeme];
    } else {
      // Only reserved words can start with special symbols in this
      // language, so tokens starting with special symbols all the way
      // up to the first non-special symbol are invalid unless they match
      // a reserved word
      returnToken.type = TokenType::TInvalid;
      std::cout << "\033[1;31mCould not tokenize: \033[0m";
    }
  } else if (currentChar == EOF) {
    std::cout << "Hit EOF" << std::endl;
  } else {
    returnToken.type = TokenType::TInvalid;

    while (isInvalidChar(currentChar)) {
      currentChar = source.get();
      returnToken.lexeme += currentChar;
    }

    returnToken.lexeme.pop_back();
    source.putback(currentChar);
    std::cout << "\033[1;31mCould not tokenize: \033[0m";
  }

  return returnToken;
}

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "You must provide exactly one argument (an input program) to the compiler" << std::endl;
    return 1;
  }

  std::ifstream source;
  source.open(argv[1], std::ifstream::in);
  if (!source.is_open()) {
    std::cerr << "Failed to open " << argv[1] << std::endl;
    return 1;
  }

  /*
   * Lexing begins here. TODO: eventually we'll want
   * to factor this out into a Lexer class that can output a single
   * token at a time.
   */
  while (source) { // Retrieve the starting character of our next token
    std::cout << getNextToken(source).lexeme << std::endl;
  }

  std::cout << "Lexing complete..." << std::endl;

  source.close();
  return 0;
}
