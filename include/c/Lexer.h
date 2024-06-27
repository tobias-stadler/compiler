#pragma once

#include <array>
#include <cassert>
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
    SPACE_START,
    SPACE,
    COMMENT,
    NEWLINE,
    SPACE_END,
    HEADER_NAME,
    LITERAL_START,
    PP_NUM,
    LITERAL_CHAR,
    LITERAL_STR,
    LITERAL_END,
    IDENTIFIER,
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
    KEYWORD_ALIGNOF,
    KEYWORD_STATIC,
    KEYWORD_STRUCT,
    KEYWORD_SWITCH,
    KEYWORD_TYPEDEF,
    KEYWORD_UNION,
    KEYWORD_UNSIGNED,
    KEYWORD_VOID,
    KEYWORD_VOLATILE,
    KEYWORD_WHILE,
    KEYWORD_BOOL,
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
    PUNCT_HASH,
    PUNCT_HASHHASH,
    PUNCT_DOTDOTDOT,
    PUNCT_END,
  };

  constexpr Token(Kind kind = EMPTY, const char *text = nullptr, size_t len = 0)
      : kind(kind), text(text), textLen(len){};

  Kind kind;
  const char *text;
  size_t textLen;

  static constexpr bool isAlphaChar(unsigned char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  }
  static constexpr bool isNumChar(unsigned char c) {
    return c >= '0' && c <= '9';
  }

  static constexpr bool isSpaceChar(unsigned char c) {
    return c == ' ' || (c >= 9 && c <= 13 && c != '\n');
  }

  static constexpr bool isPunctuator(Kind kind) {
    return kind >= PUNCT_START && kind <= PUNCT_END;
  }

  static constexpr bool isKeyword(Kind kind) {
    return kind >= KEYWORD_START && kind <= KEYWORD_END;
  }
  static constexpr bool isWhiteSpace(Kind kind) {
    return kind >= SPACE_START && kind <= SPACE_END;
  }

  static constexpr int NUM_KEYWORDS = KEYWORD_END - KEYWORD_START - 1;

  static const char *kindName(Kind kind);

  static consteval Token::Kind charKind(unsigned char c) {
    if (isNumChar(c)) {
      return PP_NUM;
    }
    if (isAlphaChar(c) || c == '_') {
      return IDENTIFIER;
    }
    if (c == '\n') {
      return NEWLINE;
    }
    if (isSpaceChar(c)) {
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
    case '#':
      return PUNCT_HASH;
    }
    return INVALID;
  }

  static_assert(sizeof(char) == 1);

  static consteval std::array<Token::Kind, 256> charTable() {
    std::array<Token::Kind, 256> a{Token::INVALID};
    for (int i = 0; i < 256; ++i) {
      a[i] = charKind(i);
    }
    return a;
  }

  operator std::string_view() const {
    assert(isValidString());
    return {text, textLen};
  }

  bool isEmpty() const;
  bool isInvalid() const;
  bool isEnd() const;
  bool isNewLine() const;
  bool isKeyword() const;
  bool isPunctuator() const;
  bool isWhiteSpace() const { return isWhiteSpace(kind); }
  bool isWhiteSpaceExceptNewLine() const {
    return kind == SPACE || kind == COMMENT;
  }

  bool isValidString() const;
};

std::ostream &operator<<(std::ostream &os, Token tok);

class Lexer {
public:
  Lexer(std::string_view input);
  Token next();

  void setEnableHeaderNames(bool v) { enableHeaderNames = v; }

private:
  static constexpr auto matchTable = Token::charTable();
  static std::unordered_map<std::string_view, Token::Kind> keywords;

  std::string_view input;
  std::string_view::iterator currPos;
  Token currTok;
  unsigned char currChar;
  bool enableHeaderNames = false;

  void getToken();
  void eatToken();
  bool getChar();
  bool getCharOrEndInvalid();
  void eatChar();
  bool hasAtLeastNChars(size_t n);

  void endToken(Token::Kind kind) {
    currTok.kind = kind;
    endToken();
  }
  void endToken() { currTok.textLen = getPosPtr() - currTok.text; }

  const char *getPosPtr();
  void probeKeyword();
  void startToken(Token::Kind kind = Token::INVALID);

  template <char DELIM> void eatLiteralStrTail() {
    while (getChar()) {
      switch (currChar) {
      case DELIM:
        eatChar();
        return;
      case '\n':
        endToken(Token::INVALID);
        return;
      case '\\':
        eatChar();
        if (!getCharOrEndInvalid())
          return;
        eatChar();
        break;
      default:
        eatChar();
        break;
      }
    }
    endToken(Token::INVALID);
  }

  void eatSpace();
  void eatSingleLineCommentTail();
  void eatMultiLineCommentTail();
  void eatIdentifier();
  void eatPPNumTail();

  template <Token::Kind BASE, Token::Kind REP = Token::EMPTY,
            Token::Kind EQ = Token::EMPTY, Token::Kind REPEQ = Token::EMPTY>
  void eatOperator() {
    static_assert(REPEQ == Token::EMPTY || REP != Token::EMPTY,
                  "REP must be set to use REPEQ");
    startToken(BASE);
    eatChar();
    if (!getChar()) {
      return;
    }
    Token::Kind kind = matchTable[currChar];

    if constexpr (BASE == Token::PUNCT_EQ) {
      switch (kind) {
      case Token::PUNCT_EQ:
        currTok.kind = Token::PUNCT_EQEQ;
        eatChar();
        return;
      default:
        return;
      }
    } else if constexpr (BASE == Token::PUNCT_HASH) {
      switch (kind) {
      case Token::PUNCT_HASH:
        currTok.kind = Token::PUNCT_HASHHASH;
        eatChar();
        return;
      default:
        return;
      }
    } else if constexpr (BASE == Token::PUNCT_MINUS) {
      switch (kind) {
      case Token::PUNCT_MINUS:
        currTok.kind = Token::PUNCT_MINUSMINUS;
        eatChar();
        return;
      case Token::PUNCT_EQ:
        currTok.kind = Token::PUNCT_MINUSEQ;
        eatChar();
        return;
      case Token::PUNCT_GT:
        currTok.kind = Token::PUNCT_ARROW;
        eatChar();
        return;
      default:
        return;
      }
    } else if constexpr (BASE == Token::PUNCT_SLASH) {
      switch (kind) {
      case Token::PUNCT_SLASH:
        currTok.kind = Token::COMMENT;
        eatChar();
        eatSingleLineCommentTail();
        return;
      case Token::PUNCT_STAR:
        currTok.kind = Token::COMMENT;
        eatChar();
        eatMultiLineCommentTail();
        return;
      case Token::PUNCT_EQ:
        currTok.kind = Token::PUNCT_SLASHEQ;
        eatChar();
        return;
      default:
        return;
      }
    } else if constexpr (BASE == Token::PUNCT_DOT) {
      switch (kind) {
      case Token::PUNCT_DOT:
        if (hasAtLeastNChars(2) && *(currPos + 1) == '.') {
          currTok.kind = Token::PUNCT_DOTDOTDOT;
          eatChar();
          eatChar();
        }
        return;
      case Token::PP_NUM:
        currTok.kind = Token::PP_NUM;
        eatPPNumTail();
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
