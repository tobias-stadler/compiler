#include <Lexer.h>
#include <ostream>
#include <utility>

Token::Token(Kind kind, const char *text, size_t len)
    : kind(kind), text(text), textLen(len){};

std::ostream &operator<<(std::ostream &os, Token tok) {
  os << Token::kindName(tok.kind);
  if (tok.isString()) {
    os << '(' << std::string_view(tok) << ')';
  }
  return os;
}

Lexer::Lexer(std::string_view input) : input(input), currPos(input.begin()) {
  getToken();
};


bool Lexer::getChar() {
  if (currPos == input.end()) {
    return false;
  }
  currChar = *currPos;
  return true;
}

bool Lexer::eatToken() {
  while (getChar()) {
    Token::Kind kind = matchTable[currChar];
    switch (kind) {
    case Token::INVALID:
      return false;
    case Token::LITERAL_NUM:
      return eatLiteralNum();
    case Token::LITERAL_STR:
      ++currPos;
      return eatLiteralStr();
    case Token::LITERAL_CHAR:
      ++currPos;
      return eatLiteralChar();
    case Token::IDENTIFIER:
      return eatIdentifier();
    case Token::SPACE:
      ++currPos;
      continue;
    case Token::PUNCT_PLUS:
      return eatOperator<Token::PUNCT_PLUS, Token::PUNCT_PLUSPLUS, Token::PUNCT_PLUSEQ>();
    case Token::PUNCT_MINUS:
      return eatOperator<Token::PUNCT_MINUS, Token::PUNCT_MINUSMINUS, Token::PUNCT_MINUSEQ>();
    case Token::PUNCT_OR:
      return eatOperator<Token::PUNCT_OR, Token::PUNCT_OROR, Token::PUNCT_OREQ>();
    case Token::PUNCT_AND:
      return eatOperator<Token::PUNCT_AND, Token::PUNCT_ANDAND, Token::PUNCT_ANDEQ>();
    case Token::PUNCT_STAR:
      return eatOperator<Token::PUNCT_STAR, Token::EMPTY, Token::PUNCT_STAREQ>();
    case Token::PUNCT_EQ:
      return eatOperator<Token::PUNCT_EQ, Token::PUNCT_EQEQ>();
    case Token::PUNCT_EXCLAMATION:
      return eatOperator<Token::PUNCT_EXCLAMATION, Token::EMPTY, Token::PUNCT_EXCLAMATIONEQ>();
    case Token::PUNCT_XOR:
      return eatOperator<Token::PUNCT_XOR, Token::EMPTY, Token::PUNCT_XOREQ>();
    case Token::PUNCT_SLASH:
      return eatOperator<Token::PUNCT_SLASH, Token::EMPTY, Token::PUNCT_SLASHEQ>();
    case Token::PUNCT_PERCENT:
      return eatOperator<Token::PUNCT_PERCENT, Token::EMPTY, Token::PUNCT_PERCENTEQ>();
    case Token::PUNCT_LT:
      return eatOperator<Token::PUNCT_LT, Token::PUNCT_LTLT, Token::PUNCT_LTEQ, Token::PUNCT_LTLTEQ>();
    case Token::PUNCT_GT:
      return eatOperator<Token::PUNCT_GT, Token::PUNCT_GTGT, Token::PUNCT_GTEQ, Token::PUNCT_GTGTEQ>();
    default:
      // Single character token
      currTok = getPosToken(kind, 1);
      ++currPos;
      return true;
    }
  }
  currTok = Token(Token::END);
  return true;
}


bool Lexer::eatLiteralNum() {
  currTok = Token(Token::LITERAL_NUM, &currPos[0], 0);
  while (getChar()) {
    switch (matchTable[currChar]) {
    case Token::LITERAL_NUM:
      ++currPos;
      currTok.textLen++;
      break;
    case Token::INVALID:
      return false;
    default:
      return true;
    }
  }
  return true;
}

bool Lexer::eatLiteralStr() {
  currTok = Token(Token::LITERAL_STR, &currPos[0], 0);
  while (getChar()) {
    switch (matchTable[currChar]) {
    case Token::LITERAL_STR:
      ++currPos;
      return true;
    default:
      ++currPos;
      ++currTok.textLen;
      break;
    }
  }
  return true;
}

bool Lexer::eatLiteralChar() {
  // currTok = Token(Token::LITERAL_CHAR, &currPos[0], 0);
  return false;
}

bool Lexer::eatIdentifier() {
  currTok = getPosToken(Token::IDENTIFIER, 0);
  while (getChar()) {
    switch (matchTable[currChar]) {
    case Token::IDENTIFIER:
    case Token::LITERAL_NUM:
      ++currPos;
      currTok.textLen++;
      break;
    case Token::INVALID:
      return false;
    default:
      return true;
    }
  }
  return true;
}

void Lexer::getToken() {
  if (currTok.isEnd()) {
    return;
  }
  if (currTok.isInvalid()) {
    currTok = Token(Token::END);
    return;
  }

  if (!eatToken()) {
    currTok = Token(Token::INVALID);
  }
}

Token Lexer::peekToken() { return currTok; }

Token Lexer::nextToken() {
  Token tok = currTok;
  getToken();
  return tok;
}

const char *Lexer::getPosPtr() { return &currPos[0]; }

Token Lexer::getPosToken(Token::Kind kind, size_t len) {
  return Token(kind, getPosPtr(), len);
}
