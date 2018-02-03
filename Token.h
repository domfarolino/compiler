#ifndef TOKEN_H
#define TOKEN_H

enum TokenType {
  LParen,
  RParen
};

class Token {
  TokenType type;
  std::string lexeme;
};

#endif
