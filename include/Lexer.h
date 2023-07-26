#pragma once

#include <array>
#include <cassert>
#include <cstdint>
#include <string_view>
#include <unordered_map>

class Token {
public:
  enum Kind {
    EMPTY,
    INVALID,
    END,
    SPACE,
    COMMENT,
    IDENTIFIER,
    LITERAL_START,
    LITERAL_NUM,
    LITERAL_CHAR,
    LITERAL_STR,
    LITERAL_END,
    KEYWORD_START,
    KEYWORD_IF,
    KEYWORD_WHILE,
    KEYWORD_FOR,
    KEYWORD_END,
    PUNCT_START,
    PUNCT_SEMICOLON,
    PUNCT_COLON,
    PUNCT_COMMA,
    PUNCT_DOT,
    PUNCT_STAR,
    PUNCT_PLUS,
    PUNCT_MINUS,
    PUNCT_SQUAREO,
    PUNCT_SQUAREC,
    PUNCT_CURLYO,
    PUNCT_CURLYC,
    PUNCT_PARENO,
    PUNCT_PARENC,
    PUNCT_EQ,
    PUNCT_AND,
    PUNCT_OR,
    PUNCT_XOR,
    PUNCT_SLASH,
    PUNCT_TILDE,
    PUNCT_LT,
    PUNCT_GT,
    PUNCT_EXCLAMATION,
    PUNCT_QUESTION,
    PUNCT_PERCENT,
    PUNCT_EQEQ,
    PUNCT_OROR,
    PUNCT_OREQ,
    PUNCT_ANDAND,
    PUNCT_ANDEQ,
    PUNCT_PLUSPLUS,
    PUNCT_PLUSEQ,
    PUNCT_MINUSMINUS,
    PUNCT_MINUSEQ,
    PUNCT_STAREQ,
    PUNCT_EXCLAMATIONEQ,
    PUNCT_XOREQ,
    PUNCT_SLASHEQ,
    PUNCT_PERCENTEQ,
    PUNCT_LTEQ,
    PUNCT_LTLT,
    PUNCT_LTLTEQ,
    PUNCT_GTEQ,
    PUNCT_GTGT,
    PUNCT_GTGTEQ,
    PUNCT_END,
  };

  Token(Kind kind = EMPTY, const char *text = nullptr, size_t len = 0);

  Kind kind;
  const char *text;
  size_t textLen;

  static constexpr bool isAlpha(unsigned char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  }
  static constexpr bool isNum(unsigned char c) { return c >= '0' && c <= '9'; }

  static constexpr bool isSpace(unsigned char c) {
    return c == ' ' || (c >= 9 && c <= 13);
  }

  static constexpr const char *kindName(Kind kind) {
    switch (kind) {
    case EMPTY:
      return "Empty";
    case INVALID:
      return "Invalid";
    case END:
      return "EOF";
    case SPACE:
      return "Space";
    case COMMENT:
      return "Comment";
    case KEYWORD_WHILE:
      return "Keyword";
    case IDENTIFIER:
      return "Ident";
    case PUNCT_MINUS:
      return "Punct";
    case LITERAL_NUM:
      return "Num";
    case LITERAL_CHAR:
      return "Char";
    case LITERAL_STR:
      return "Str";
    default:
      break;
    }
    return "Unnamed";
  }

  static consteval Token::Kind charKind(unsigned char c) {
    if (isNum(c)) {
      return LITERAL_NUM;
    }
    if (isAlpha(c)) {
      return IDENTIFIER;
    }
    if (isSpace(c)) {
      return SPACE;
    }
    switch (c) {
    case '\'':
      return LITERAL_CHAR;
    case '"':
      return LITERAL_STR;
    case ';':
      return PUNCT_SEMICOLON;
    case ',':
      return PUNCT_COMMA;
    case ':':
      return PUNCT_COLON;
    case '.':
      return PUNCT_DOT;
    case '*':
      return PUNCT_STAR;
    case '+':
      return PUNCT_PLUS;
    case '-':
      return PUNCT_MINUS;
    case '(':
      return PUNCT_PARENO;
    case ')':
      return PUNCT_PARENC;
    case '[':
      return PUNCT_SQUAREO;
    case ']':
      return PUNCT_SQUAREC;
    case '{':
      return PUNCT_CURLYO;
    case '}':
      return PUNCT_CURLYC;
    case '=':
      return PUNCT_EQ;
    case '/':
      return PUNCT_SLASH;
    case '&':
      return PUNCT_AND;
    case '|':
      return PUNCT_OR;
    case '<':
      return PUNCT_LT;
    case '>':
      return PUNCT_GT;
    case '!':
      return PUNCT_EXCLAMATION;
    case '^':
      return PUNCT_XOR;
    case '~':
      return PUNCT_TILDE;
    case '%':
      return PUNCT_PERCENT;
    case '?':
      return PUNCT_QUESTION;
    }
    return INVALID;
  }

  static consteval std::array<Token::Kind, 256> charTable() {
    std::array<Token::Kind, 256> a{Token::INVALID};
    for (int i = 0; i < 256; ++i) {
      a[i] = charKind(i);
    }
    return a;
  }

  explicit operator std::string_view() const { return {text, textLen}; }

  bool isEmpty() const { return kind == EMPTY; }
  bool isInvalid() const { return kind == INVALID; }
  bool isEnd() const { return kind == END; }
  bool isKeyword() const {
    return kind >= KEYWORD_START && kind <= KEYWORD_END;
  }
  bool isPunctuator() const { return kind >= PUNCT_START && kind <= PUNCT_END; }

  bool isString() const { return kind != INVALID && text != nullptr; }
};

std::ostream &operator<<(std::ostream &os, Token tok);

class Lexer {
public:
  Lexer(std::string_view input);
  Token nextToken();
  Token peekToken();
  bool matchToken(Token tok);

private:
  static constexpr auto matchTable = Token::charTable();

  std::string_view input;
  std::string_view::iterator currPos;
  Token currTok;
  unsigned char currChar;
  // std::unordered_map<std::string, Token::Kind> keywords;

  void getToken();
  bool eatToken();
  bool getChar();
  bool eatLiteralNum();
  bool eatLiteralStr();
  bool eatPlus();
  bool eatMinus();
  bool eatIdentifier();
  bool eatLiteralChar();
  bool eatPunctuator();
  const char *getPosPtr();

  template <Token::Kind BASE, Token::Kind REP = Token::EMPTY,
            Token::Kind EQ = Token::EMPTY, Token::Kind REPEQ = Token::EMPTY>
  bool eatOperator() {
    static_assert(REPEQ == Token::EMPTY || REP != Token::EMPTY,
                  "REP must be set to use REPEQ");
    currTok = getPosToken(BASE, 1);
    ++currPos;
    if (!getChar()) {
      return true;
    }
    Token::Kind kind = matchTable[currChar];

    if constexpr (BASE == Token::PUNCT_EQ) {
      switch (kind) {
      case Token::INVALID:
        return false;
      case BASE:
        if constexpr (REP != Token::EMPTY) {
          currTok.kind = REP;
          break;
        }
        return true;
      default:
        return true;
      }
    } else {
      switch (kind) {
      case Token::INVALID:
        return false;
      case BASE:
        if constexpr (REP != Token::EMPTY) {
          currTok.kind = REP;
          break;
        }
        return true;
      case Token::PUNCT_EQ:
        if constexpr (EQ != Token::EMPTY) {
          currTok.kind = EQ;
          ++currTok.textLen;
          ++currPos;
        }
        return true;
      default:
        return true;
      }
    }

    ++currTok.textLen;
    ++currPos;
    if constexpr (REPEQ == Token::EMPTY) {
      return true;
    }
    if (!getChar()) {
      return true;
    }
    kind = matchTable[currChar];
    switch (kind) {
    case Token::INVALID:
      return false;
    case Token::PUNCT_EQ:
      currTok.kind = REPEQ;
      ++currTok.textLen;
      ++currPos;
      return true;
    default:
      return true;
    }
  }
  Token getPosToken(Token::Kind kind, size_t len);
};
