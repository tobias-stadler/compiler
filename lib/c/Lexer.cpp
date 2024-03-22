#include "c/Lexer.h"
#include <cassert>
#include <string_view>

namespace c {

std::ostream &operator<<(std::ostream &os, Token tok) {
  if (tok.isPunctuator() || tok.isKeyword()) {
    os << "'" << std::string_view(tok) << "'";
    return os;
  }
  os << Token::kindName(tok.kind);
  if (tok.kind == Token::NEWLINE) {
    return os;
  }
  if (tok.isValidString()) {
    os << "'" << std::string_view(tok) << "'";
  }
  return os;
}

Lexer::Lexer(std::string_view input) : input(input), currPos(input.begin()){};

bool Lexer::hasAtLeastNChars(size_t n) {
  return (size_t)std::distance(currPos, input.end()) >= n;
}

bool Lexer::getChar() {
  if (currPos == input.end()) {
    return false;
  }
  currChar = *currPos;
  return true;
}

bool Lexer::getCharOrEndInvalid() {
  if (getChar()) {
    return true;
  }
  endToken(Token::INVALID);
  return false;
}

std::unordered_map<std::string_view, Token::Kind> Lexer::keywords = {
    {"u8", Token::LITERAL_STR},
    {"u", Token::LITERAL_CHAR},
    {"U", Token::LITERAL_CHAR},
    {"L", Token::LITERAL_CHAR},
};

void Lexer::eatToken() {
  while (getChar()) {
    Token::Kind kind = matchTable[currChar];
    switch (kind) {
    default:
      startToken(Token::INVALID);
      eatChar();
      return;
    case Token::LITERAL_STR:
      if (enableHeaderNames) {
        startToken(Token::HEADER_NAME);
      } else {
        startToken(Token::LITERAL_STR);
      }
      eatChar();
      eatLiteralStrTail<'"'>();
      return;
    case Token::LITERAL_CHAR:
      startToken(Token::LITERAL_CHAR);
      eatChar();
      eatLiteralStrTail<'\''>();
      return;
    case Token::IDENTIFIER:
      eatIdentifier();
      probeKeyword();
      return;
    case Token::PP_NUM:
      startToken(Token::PP_NUM);
      eatPPNumTail();
      return;
    case Token::SPACE:
      eatSpace();
      return;
    case Token::PUNCT_PLUS:
      eatOperator<Token::PUNCT_PLUS, Token::PUNCT_PLUSPLUS,
                  Token::PUNCT_PLUSEQ>();
      return;
    case Token::PUNCT_MINUS:
      eatOperator<Token::PUNCT_MINUS>();
      return;
    case Token::PUNCT_OR:
      eatOperator<Token::PUNCT_OR, Token::PUNCT_OROR, Token::PUNCT_OREQ>();
      return;
    case Token::PUNCT_AND:
      eatOperator<Token::PUNCT_AND, Token::PUNCT_ANDAND, Token::PUNCT_ANDEQ>();
      return;
    case Token::PUNCT_STAR:
      eatOperator<Token::PUNCT_STAR, Token::EMPTY, Token::PUNCT_STAREQ>();
      return;
    case Token::PUNCT_EQ:
      eatOperator<Token::PUNCT_EQ>();
      return;
    case Token::PUNCT_EXCLAMATION:
      eatOperator<Token::PUNCT_EXCLAMATION, Token::EMPTY,
                  Token::PUNCT_EXCLAMATIONEQ>();
      return;
    case Token::PUNCT_XOR:
      eatOperator<Token::PUNCT_XOR, Token::EMPTY, Token::PUNCT_XOREQ>();
      return;
    case Token::PUNCT_SLASH:
      eatOperator<Token::PUNCT_SLASH>();
      return;
    case Token::PUNCT_PERCENT:
      eatOperator<Token::PUNCT_PERCENT, Token::EMPTY, Token::PUNCT_PERCENTEQ>();
      return;
    case Token::PUNCT_LT:
      if (enableHeaderNames) {
        startToken(Token::HEADER_NAME);
        eatChar();
        eatLiteralStrTail<'>'>();
      } else {
        eatOperator<Token::PUNCT_LT, Token::PUNCT_LTLT, Token::PUNCT_LTEQ,
                    Token::PUNCT_LTLTEQ>();
      }
      return;
    case Token::PUNCT_GT:
      eatOperator<Token::PUNCT_GT, Token::PUNCT_GTGT, Token::PUNCT_GTEQ,
                  Token::PUNCT_GTGTEQ>();
      return;
    case Token::PUNCT_DOT:
      eatOperator<Token::PUNCT_DOT>();
      return;
    case Token::PUNCT_HASH:
      eatOperator<Token::PUNCT_HASH>();
      return;
    case Token::PUNCT_CURLYO:
    case Token::PUNCT_CURLYC:
    case Token::PUNCT_SQUAREO:
    case Token::PUNCT_SQUAREC:
    case Token::PUNCT_PARENO:
    case Token::PUNCT_PARENC:
    case Token::PUNCT_COLON:
    case Token::PUNCT_SEMICOLON:
    case Token::PUNCT_QUESTION:
    case Token::PUNCT_COMMA:
    case Token::PUNCT_TILDE:
    case Token::NEWLINE:
      startToken(kind);
      eatChar();
      return;
    }
  }
  currTok = Token(Token::END);
}

void Lexer::eatSingleLineCommentTail() {
  while (getChar()) {
    if (currChar == '\n')
      break;
    eatChar();
  }
}

void Lexer::eatMultiLineCommentTail() {
  bool wasStar = false;
  while (getChar()) {
    eatChar();
    if (wasStar && currChar == '/') {
      break;
    }
    wasStar = false;
    if (currChar == '*') {
      wasStar = true;
    }
  }
}

void Lexer::eatSpace() {
  startToken(Token::SPACE);
  while (getChar()) {
    if (Token::isSpaceChar(currChar)) {
      eatChar();
    } else {
      return;
    }
  }
}

void Lexer::eatIdentifier() {
  startToken(Token::IDENTIFIER);
  while (getChar()) {
    if (Token::isAlphaChar(currChar) || Token::isNumChar(currChar) ||
        currChar == '_') {
      eatChar();
    } else {
      return;
    }
  }
}

void Lexer::eatPPNumTail() {
  bool nextSign = false;
  while (getChar()) {
    if (Token::isNumChar(currChar) || Token::isAlphaChar(currChar) ||
        currChar == '_' || currChar == '.' ||
        (nextSign && (currChar == '+' || currChar == '-'))) {
      if (currChar == 'e' || currChar == 'E' || currChar == 'p' ||
          currChar == 'P') {
        nextSign = true;
      } else {
        nextSign = false;
      }
      eatChar();
    } else {
      break;
    }
  }
}

void Lexer::probeKeyword() {
  auto it = keywords.find(std::string_view(currTok));
  if (it == keywords.end())
    return;
  auto kind = it->second;
  if (kind == Token::LITERAL_STR || kind == Token::LITERAL_CHAR) {
    if (!getChar()) {
      return;
    }
    switch (currChar) {
    case '\'':
      if (kind == Token::LITERAL_CHAR) {
        currTok.kind = Token::LITERAL_CHAR;
        eatChar();
        eatLiteralStrTail<'\''>();
      }
      break;
    case '"':
      currTok.kind = Token::LITERAL_STR;
      eatChar();
      eatLiteralStrTail<'"'>();
      break;
    }
  } else {
    currTok.kind = kind;
  }
}

void Lexer::getToken() { eatToken(); }

Token Lexer::next() {
  getToken();
  return currTok;
}

const char *Lexer::getPosPtr() { return &currPos[0]; }

void Lexer::startToken(Token::Kind kind) { currTok = Token(kind, getPosPtr()); }

bool Token::isEmpty() const { return kind == EMPTY; }

bool Token::isInvalid() const { return kind == INVALID; }

bool Token::isEnd() const { return kind == END; }

bool Token::isNewLine() const { return kind == NEWLINE; }

bool Token::isKeyword() const { return isKeyword(kind); }

bool Token::isPunctuator() const { return isPunctuator(kind); }

bool Token::isValidString() const { return text && textLen > 0; }

void Lexer::eatChar() {
  ++currPos;
  ++currTok.textLen;
}

const char *Token::kindName(Kind kind) {
  switch (kind) {
  case END:
    return "<end>";
  case SPACE:
    return "<space>";
  case COMMENT:
    return "<comment>";
  case IDENTIFIER:
    return "<identifier>";
  case PP_NUM:
    return "<pp-num>";
  case LITERAL_CHAR:
    return "<char>";
  case LITERAL_STR:
    return "<string>";
  case KEYWORD_AUTO:
    return "auto";
  case KEYWORD_BREAK:
    return "break";
  case KEYWORD_CASE:
    return "case";
  case KEYWORD_CHAR:
    return "char";
  case KEYWORD_CONST:
    return "const";
  case KEYWORD_CONTINUE:
    return "continue";
  case KEYWORD_DEFAULT:
    return "default";
  case KEYWORD_DO:
    return "do";
  case KEYWORD_DOUBLE:
    return "double";
  case KEYWORD_ELSE:
    return "else";
  case KEYWORD_ENUM:
    return "enum";
  case KEYWORD_EXTERN:
    return "extern";
  case KEYWORD_FLOAT:
    return "float";
  case KEYWORD_FOR:
    return "for";
  case KEYWORD_GOTO:
    return "goto";
  case KEYWORD_IF:
    return "if";
  case KEYWORD_INLINE:
    return "inline";
  case KEYWORD_INT:
    return "int";
  case KEYWORD_LONG:
    return "long";
  case KEYWORD_REGISTER:
    return "register";
  case KEYWORD_RESTRICT:
    return "restrict";
  case KEYWORD_RETURN:
    return "return";
  case KEYWORD_SHORT:
    return "short";
  case KEYWORD_SIGNED:
    return "signed";
  case KEYWORD_SIZEOF:
    return "sizeof";
  case KEYWORD_STATIC:
    return "static";
  case KEYWORD_STRUCT:
    return "struct";
  case KEYWORD_SWITCH:
    return "switch";
  case KEYWORD_TYPEDEF:
    return "typedef";
  case KEYWORD_UNION:
    return "union";
  case KEYWORD_UNSIGNED:
    return "unsigned";
  case KEYWORD_VOID:
    return "void";
  case KEYWORD_VOLATILE:
    return "volatile";
  case KEYWORD_WHILE:
    return "while";
  case KEYWORD_BOOL:
    return "bool";
  case KEYWORD_ALIGNOF:
    return "alignof";
  case PUNCT_SEMICOLON:
    return ";";
  case PUNCT_COLON:
    return ":";
  case PUNCT_COMMA:
    return ",";
  case PUNCT_DOT:
    return ".";
  case PUNCT_STAR:
    return "*";
  case PUNCT_PLUS:
    return "+";
  case PUNCT_MINUS:
    return "-";
  case PUNCT_SQUAREO:
    return "[";
  case PUNCT_SQUAREC:
    return "]";
  case PUNCT_CURLYO:
    return "{";
  case PUNCT_CURLYC:
    return "}";
  case PUNCT_PARENO:
    return "(";
  case PUNCT_PARENC:
    return ")";
  case PUNCT_EQ:
    return "==";
  case PUNCT_AND:
    return "&";
  case PUNCT_OR:
    return "|";
  case PUNCT_XOR:
    return "^";
  case PUNCT_SLASH:
    return "/";
  case PUNCT_TILDE:
    return "~";
  case PUNCT_LT:
    return "<";
  case PUNCT_GT:
    return ">";
  case PUNCT_EXCLAMATION:
    return "!";
  case PUNCT_QUESTION:
    return "?";
  case PUNCT_PERCENT:
    return "%";
  case PUNCT_EQEQ:
    return "==";
  case PUNCT_OROR:
    return "||";
  case PUNCT_OREQ:
    return "|=";
  case PUNCT_ANDAND:
    return "&&";
  case PUNCT_ANDEQ:
    return "&=";
  case PUNCT_PLUSPLUS:
    return "++";
  case PUNCT_PLUSEQ:
    return "+=";
  case PUNCT_MINUSMINUS:
    return "--";
  case PUNCT_MINUSEQ:
    return "-=";
  case PUNCT_STAREQ:
    return "*=";
  case PUNCT_EXCLAMATIONEQ:
    return "!=";
  case PUNCT_XOREQ:
    return "^=";
  case PUNCT_SLASHEQ:
    return "/=";
  case PUNCT_PERCENTEQ:
    return "%=";
  case PUNCT_LTEQ:
    return "<=";
  case PUNCT_LTLT:
    return "<<";
  case PUNCT_LTLTEQ:
    return "<<=";
  case PUNCT_GTEQ:
    return ">=";
  case PUNCT_GTGT:
    return ">>";
  case PUNCT_GTGTEQ:
    return ">>=";
  case PUNCT_ARROW:
    return "->";
  case EMPTY:
    return "<empty>";
  case INVALID:
    return "<invalid>";
  case NEWLINE:
    return "<newline>";
  case HEADER_NAME:
    return "<header>";
  case PUNCT_HASH:
    return "#";
  case PUNCT_HASHHASH:
    return "##";
  case PUNCT_DOTDOTDOT:
    return "...";
  case LITERAL_START:
  case LITERAL_END:
  case KEYWORD_START:
  case KEYWORD_END:
  case PUNCT_START:
  case PUNCT_END:
  case SPACE_START:
  case SPACE_END:
    break;
  }
  return "<illegal>";
}
} // namespace c
