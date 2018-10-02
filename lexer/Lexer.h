#ifndef LEXER_H
#define LEXER_H

#include <fstream>
#include <string>
#include <unordered_map>

#include "../lib/Token.h"

class Lexer {
public:
  std::ifstream source;
  int lineNumber;
  bool done = false;
  std::unordered_map<std::string, TokenType> reservedWords;

  Lexer(const std::string&, bool);

  Token nextToken();

  bool isDone() {
    return done;
  }

  ~Lexer();

private:
  bool verbose_;
  bool isAlphabetCharacter(char c) {
    return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
  }

  bool isSpecialCharacter(char c) {
    return (c == '.' || c == ';' ||
            c == '(' || c == ')' ||
            c == '[' || c == ']' ||
            c == '&' || c == '|' ||
            c == '+' || c == '-' ||
            c == ':' || c == '<' ||
            c == '>' || c == '=' ||
            c == '!' || c == '*' ||
            c == '/' || c == ',');
  }

  bool isDigit(char c) {
    return ('0' <= c && c <= '9');
  }

  bool isWhitespace(char c) {
    return (c == ' ' || isNewline(c) || c == '\t');
  }

  bool isDoubleQuote(char c) {
    return c == '"';
  }

  bool isSingleQuote(char c) {
    return c == '\'';
  }

  bool isInvalidCharacter(char c) {
    return !isAlphabetCharacter(c) && !isSpecialCharacter(c) && !isDigit(c) && !isWhitespace(c) && c != EOF;
  }

  bool isNewline(char c) {
    // TODO: handle other newline characters
    return c == '\n';
  }

  Token processIdentifierOrReservedWord();
  Token processString();
  Token processCharacter();
  Token processIntegerOrFloat();
  Token processSpecialCharacter();
  Token processEOF();
  Token processInvalid();
  void processWhitespace();

  void displayLexerError(Token);
  void displayLexerWarning(Token);

};

#endif
