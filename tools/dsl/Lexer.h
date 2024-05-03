#pragma once

#include <optional>
#include <string_view>
#include <vector>

[[noreturn]] void error(std::string_view err);

class Token {
public:
  enum Kind {
    EMPTY,
    CUSTOM,
    CODE,
    STR,
    IDENT,
    PARENO,
    PARENC,
    CURLYO,
    CURLYC,
    SQUAREO,
    SQUAREC,
    EQ,
    COMMA,
    COLON,
    LT,
    GT,
    SLASH_FWD,
    DOT,
    SEMICOLON,
    PERCENT,
    HASH,
    AT,
    EXCLAM,
  };
  Kind kind = EMPTY;
  std::string_view str;

  Token() {}

  Token(Kind kind, std::string_view str) : kind(kind), str(str) {}

  operator std::string_view() { return str; }

  explicit operator bool() { return kind != EMPTY; }
};

class RecordIdent {
public:
  RecordIdent() {}
  RecordIdent(std::vector<std::string_view> idents)
      : idents(std::move(idents)) {}

  std::vector<std::string_view> idents;

  bool empty() { return idents.empty() || idents.back() == "_"; }

  std::string_view getName() { return idents.back(); }
};

class RecordHdr {
public:
  std::string_view name;
  std::string_view type;
  bool isTemplateInstance = false;
};

class TokenSource {
public:
  virtual ~TokenSource() {}
  virtual Token fetchToken() = 0;
  virtual void dump(Token tok) {}
};

class StringTokenSource : public TokenSource {
public:
  std::string_view str;
  std::string_view::iterator pos;
  std::vector<const char *> newlines;

  StringTokenSource(std::string_view str) : str(str), pos(str.begin()) {}

  Token fetchToken() override;
  void dump(Token tok) override;

  Token::Kind getSingleCharKind();

  bool eatWhitespace();

  std::optional<size_t> getLineNum(std::string_view str);
};

class Lexer {
public:
  TokenSource &tokSrc;
  Token tok;

  Lexer(TokenSource &tokSrc) : tokSrc(tokSrc) { fetchToken(); }

  [[noreturn]] void error(std::string_view err, Token tok);
  [[noreturn]] void error(std::string_view err) { error(err, tok); }

  void fetchToken() { tok = tokSrc.fetchToken(); }

  Token peek() { return tok; }
  Token::Kind peekKind() { return tok.kind; }
  Token next();

  bool match(Token::Kind kind);
  bool matchOrEmpty(Token::Kind kind);
  bool matchNext(Token::Kind kind);
  bool matchOrEmptyNext(Token::Kind kind);
  Token expectNext(Token::Kind kind);
  void expectIdentNext(std::string_view ident);
  bool matchIdent(std::string_view ident);
  bool matchIdentNext(std::string_view ident);
  Token expectSSAName();
  Token expectPlaceholderName();
  void expectDoubleColon();
  RecordIdent expectRecordIdent();
  RecordHdr expectRecordHdr();
  std::vector<Token> expectCode();
};
