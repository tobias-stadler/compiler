#pragma once

#include <array>
#include <cassert>
#include <cstdint>
#include <ostream>
#include <string_view>
#include <unordered_map>

namespace c {

class Token {
public:
  enum Kind {
    EMPTY,
    INVALID,
    END,
    SPACE,
    ESCAPE,
    COMMENT,
    IDENTIFIER,
    LITERAL_START,
    LITERAL_NUM,
    LITERAL_CHAR,
    LITERAL_STR,
    LITERAL_END,
    KEYWORD_START,
    KEYWORD_AUTO,
    KEYWORD_BREAK,
    KEYWORD_CASE,
    KEYWORD_CHAR,
    KEYWORD_CONST,
    KEYWORD_CONTINUE,
    KEYWORD_DEFAULT,
    KEYWORD_DO,
    KEYWORD_DOUBLE,
    KEYWORD_ELSE,
    KEYWORD_ENUM,
    KEYWORD_EXTERN,
    KEYWORD_FLOAT,
    KEYWORD_FOR,
    KEYWORD_GOTO,
    KEYWORD_IF,
    KEYWORD_INLINE,
    KEYWORD_INT,
    KEYWORD_LONG,
    KEYWORD_REGISTER,
    KEYWORD_RESTRICT,
    KEYWORD_RETURN,
    KEYWORD_SHORT,
    KEYWORD_SIGNED,
    KEYWORD_SIZEOF,
    KEYWORD_STATIC,
    KEYWORD_STRUCT,
    KEYWORD_SWITCH,
    KEYWORD_TYPEDEF,
    KEYWORD_UNION,
    KEYWORD_UNSIGNED,
    KEYWORD_VOID,
    KEYWORD_VOLATILE,
    KEYWORD_WHILE,
    KEYWORD__BOOL,
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
    PUNCT_ARROW,
    PUNCT_END,
    TOKEN_END,
  };

  constexpr Token(Kind kind = EMPTY, const char *text = nullptr, size_t len = 0)
      : kind(kind), text(text), textLen(len){};

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

  static constexpr bool isPunctuator(Kind kind) {
    return kind >= PUNCT_START && kind <= PUNCT_END;
  }

  static constexpr bool isKeyword(Kind kind) {
    return kind >= KEYWORD_START && kind <= KEYWORD_END;
  }

  static constexpr int NUM_KEYWORDS = KEYWORD_END - KEYWORD_START - 1;

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
    case IDENTIFIER:
      return "Ident";
    case LITERAL_NUM:
      return "Num";
    case LITERAL_CHAR:
      return "Char";
    case LITERAL_STR:
      return "Str";
    default:
      break;
    }
    if (isPunctuator(kind)) {
      return "Punct";
    }
    if (isKeyword(kind)) {
      return "Keyword";
    }
    return "Unnamed";
  }

  static consteval Token::Kind charKind(unsigned char c) {
    if (isNum(c)) {
      return LITERAL_NUM;
    }
    if (isAlpha(c) || c == '_') {
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
    case '\\':
      return ESCAPE;
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

  operator std::string_view() const {
    if (text == nullptr)
      return {};
    return {text, textLen};
  }

  bool isEmpty() const;
  bool isInvalid() const;
  bool isEnd() const;
  bool isKeyword() const;
  bool isPunctuator() const;

  bool isValidString() const;
};

std::ostream &operator<<(std::ostream &os, Token tok);

class Lexer {
public:
  Lexer(std::string_view input);
  Token nextToken();
  Token peekToken();
  void dropToken();
  Token::Kind peekTokenKind();
  bool matchNextToken(Token::Kind kind);
  bool matchPeekToken(Token::Kind kind);

private:
  static constexpr auto matchTable = Token::charTable();
  static std::unordered_map<std::string_view, Token::Kind> keywords;

  std::string_view input;
  std::string_view::iterator currPos;
  Token currTok;
  unsigned char currChar;

  void getToken();
  void eatToken();
  bool getChar();
  void eatChar();
  void dropChar();

  const char *getPosPtr();
  void probeKeyword();
  void getPosToken(Token::Kind kind);

  template <Token::Kind DELIM> void eatLiteralStr() {
    dropChar();
    getPosToken(DELIM);
    while (getChar()) {
      switch (matchTable[currChar]) {
      case DELIM:
        dropChar();
        return;
      case Token::ESCAPE:
        eatChar();
        if (!getChar())
          break;
        eatChar();
        break;
      default:
        eatChar();
        break;
      }
    }
    currTok.kind = Token::INVALID;
  }

  void eatIdentifier(Token::Kind kind);

  template <Token::Kind BASE, Token::Kind REP = Token::EMPTY,
            Token::Kind EQ = Token::EMPTY, Token::Kind REPEQ = Token::EMPTY>
  void eatOperator() {
    static_assert(REPEQ == Token::EMPTY || REP != Token::EMPTY,
                  "REP must be set to use REPEQ");
    getPosToken(BASE);
    eatChar();
    if (!getChar()) {
      return;
    }
    Token::Kind kind = matchTable[currChar];

    if constexpr (BASE == Token::PUNCT_EQ) {
      switch (kind) {
      case BASE:
        if constexpr (REP != Token::EMPTY) {
          currTok.kind = REP;
          break;
        }
        return;
      default:
        return;
      }
    } else if constexpr (BASE == Token::PUNCT_MINUS) {
      switch (kind) {
      case BASE:
        if constexpr (REP != Token::EMPTY) {
          currTok.kind = REP;
          break;
        }
        return;
      case Token::PUNCT_EQ:
        if constexpr (EQ != Token::EMPTY) {
          currTok.kind = EQ;
          eatChar();
        }
        return;
      case Token::PUNCT_GT:
        currTok.kind = Token::PUNCT_ARROW;
        eatChar();
        return;
      default:
        return;
      }
    } else {
      switch (kind) {
      case BASE:
        if constexpr (REP != Token::EMPTY) {
          currTok.kind = REP;
          break;
        }
        return;
      case Token::PUNCT_EQ:
        if constexpr (EQ != Token::EMPTY) {
          currTok.kind = EQ;
          eatChar();
        }
        return;
      default:
        return;
      }
    }
    eatChar();

    // Third character
    if constexpr (REPEQ == Token::EMPTY) {
      return;
    }
    if (!getChar()) {
      return;
    }
    kind = matchTable[currChar];
    switch (kind) {
    case Token::PUNCT_EQ:
      currTok.kind = REPEQ;
      eatChar();
      return;
    default:
      return;
    }
  }
};
} // namespace c
