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

bool isOneCharReservedSymbol(char c) {
  return true;
}

bool isDigit(char c) {
  return ('0' <= c && c <= '9');
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
  while (currentChar == ' ' || currentChar == '\t') {
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

  if (isUpper(currentChar)) {
    // Token is an identifier
    std::cout << "Tokenizing identifer" << std::endl;
    returnToken.type = TokenType::TIdentifier;

    // Build lexeme
    while (std::regex_match(returnToken.lexeme, std::regex("[a-zA-Z][a-zA-Z0-9_]*"))) {
      currentChar = source.get();
      returnToken.lexeme += currentChar;
    }

    returnToken.lexeme.pop_back();
    source.putback(currentChar);
  } else if (isLower(currentChar)) {
    // Token is either reserved word, or identifier
    std::cout << "Tokenizing reserved word or identifier" << std::endl;

    // Build lexeme
  } else if (isDigit(currentChar)) {
    // Token is a number
    std::cout << "Tokenizing number" << std::endl;

    // Build lexeme
    while (std::regex_match(returnToken.lexeme, std::regex("[0-9][0-9_]*\\.?[0-9_]*"))) { // TODO: verify this regex (it is a modification of the spec)
      currentChar = source.get();
      returnToken.lexeme += currentChar;
    }

    returnToken.lexeme.pop_back();
    source.putback(currentChar);
    returnToken.type = (returnToken.lexeme.find('.') == std::string::npos) ? TokenType::TInteger : TokenType::TFloat;
  } else if (currentChar == EOF) {
    std::cout << "Hit EOF" << std::endl;
  } else {
    std::cout << "Could not find a matching rule for " << currentChar << std::endl;
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
