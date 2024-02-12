#include "Lexer.h"

#include <cstdlib>
#include <format>
#include <iostream>
#include <regex>

void error(std::string_view err) {
  std::cerr << "[Error] " << err << std::endl;
  std::exit(EXIT_FAILURE);
}

Token StringTokenSource::fetchToken() {
  Token tok;
  if (!eatWhitespace()) {
    return tok;
  }

  auto kind = getSingleCharKind();
  if (kind != Token::EMPTY) {
    tok.kind = kind;
    auto tmp = pos++;
    tok.str = std::string_view(tmp, pos);
    return tok;
  }

  static const std::regex identRegEx("([a-zA-Z_0-9]+)");
  static const std::regex strRegEx("\"(.*)\"");
  static const std::regex codeRegEx("[$]([^$]+)[$]");

  std::cmatch m;
  if (std::regex_search(pos, str.end(), m, identRegEx,
                        std::regex_constants::match_continuous)) {
    kind = Token::IDENT;
  } else if (std::regex_search(pos, str.end(), m, strRegEx,
                               std::regex_constants::match_continuous)) {
    kind = Token::STR;
  } else if (std::regex_search(pos, str.end(), m, codeRegEx,
                               std::regex_constants::match_continuous)) {
    kind = Token::CODE;
  }
  if (kind != Token::EMPTY) {
    tok.str = std::string_view(m[1].first, m[1].second);
    tok.kind = kind;
    pos = m[0].second;
    return tok;
  }
  error(std::format("Invalid token: {}", *pos));
}

Token::Kind StringTokenSource::getSingleCharKind() {
  switch (*pos) {
  case '{':
    return Token::CURLYO;
  case '}':
    return Token::CURLYC;
  case '(':
    return Token::PARENO;
  case ')':
    return Token::PARENC;
  case '[':
    return Token::SQUAREO;
  case ']':
    return Token::SQUAREC;
  case '=':
    return Token::EQ;
  case ':':
    return Token::COLON;
  case ',':
    return Token::COMMA;
  case ';':
    return Token::SEMICOLON;
  case '%':
    return Token::PERCENT;
  case '#':
    return Token::HASH;
  case '/':
    return Token::SLASH_FWD;
  case '.':
    return Token::DOT;
  case '<':
    return Token::LT;
  case '>':
    return Token::GT;
  case '@':
    return Token::AT;
  case '!':
    return Token::EXCLAM;
  default:
    return Token::EMPTY;
  }
}

bool StringTokenSource::eatWhitespace() {
  while (true) {
    if (pos == str.end()) {
      return false;
    }
    if (isspace(*pos)) {
      ++pos;
    } else {
      return true;
    }
  }
}

Token Lexer::next() {
  Token tmp = tok;
  fetchToken();
  return tmp;
}

bool Lexer::match(Token::Kind kind) {
  if (tok.kind != kind)
    return false;

  return true;
}

bool Lexer::matchOrEmpty(Token::Kind kind) {
  if (tok.kind == kind || tok.kind == Token::EMPTY) {
    return true;
  }
  return false;
}

bool Lexer::matchNext(Token::Kind kind) {
  if (tok.kind != kind)
    return false;

  fetchToken();
  return true;
}

bool Lexer::matchOrEmptyNext(Token::Kind kind) {
  if (tok.kind != kind && tok.kind != Token::EMPTY)
    return false;

  fetchToken();
  return true;
}

Token Lexer::expectNext(Token::Kind kind) {
  if (tok.kind != kind)
    error(std::format("Expected {} token. Got: {} {}", (int)kind, (int)tok.kind,
                      tok.str));

  return next();
}

void Lexer::expectIdentNext(std::string_view ident) {
  if (expectNext(Token::IDENT) != ident) {
    error("Expected different ident");
  }
}

bool Lexer::matchIdent(std::string_view ident) {
  if (tok.kind == Token::IDENT && tok == ident) {
    return true;
  }
  return false;
}

bool Lexer::matchIdentNext(std::string_view ident) {
  if (tok.kind == Token::IDENT && tok == ident) {
    fetchToken();
    return true;
  }
  return false;
}

Token Lexer::expectSSAName() {
  expectNext(Token::PERCENT);
  return expectNext(Token::IDENT);
}

Token Lexer::expectPlaceholderName() {
  expectNext(Token::HASH);
  return expectNext(Token::IDENT);
}

void Lexer::expectDoubleColon() {
  expectNext(Token::COLON);
  expectNext(Token::COLON);
}

RecordIdent Lexer::expectRecordIdent() {
  std::vector<std::string_view> res;
  res.push_back(expectNext(Token::IDENT));
  while (match(Token::COLON)) {
    expectDoubleColon();
    res.push_back(expectNext(Token::IDENT));
  }
  return res;
}

RecordHdr Lexer::expectRecordHdr() {
  RecordHdr res;
  if (matchIdentNext("let")) {
    res.name = expectNext(Token::IDENT);
    expectNext(Token::EQ);
  }
  if (matchNext(Token::EXCLAM)) {
    res.isTemplateInstance = true;
  }
  res.type = expectNext(Token::IDENT);
  return res;
}

std::vector<Token> Lexer::expectCode() {
  std::vector<Token> code;
  while (true) {
    switch (peekKind()) {
    default:
      return code;
    case Token::CODE:
      code.push_back(next());
      break;
    case Token::HASH:
    case Token::PERCENT:
    case Token::IDENT:
      code.push_back(next());
      break;
    }
  }
}
