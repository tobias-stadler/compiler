#include "c/Lexer.h"
#include <string_view>

namespace c {

std::ostream &operator<<(std::ostream &os, Token tok) {
  os << Token::kindName(tok.kind);
  if (tok.isValidString()) {
    os << " \'" << std::string_view(tok) << '\'';
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

std::unordered_map<std::string_view, Token::Kind> Lexer::keywords = {
    {"while", Token::KEYWORD_WHILE},
    {"volatile", Token::KEYWORD_VOLATILE},
    {"void", Token::KEYWORD_VOID},
    {"unsigned", Token::KEYWORD_UNSIGNED},
    {"union", Token::KEYWORD_UNION},
    {"typedef", Token::KEYWORD_TYPEDEF},
    {"switch", Token::KEYWORD_SWITCH},
    {"struct", Token::KEYWORD_STRUCT},
    {"static", Token::KEYWORD_STATIC},
    {"sizeof", Token::KEYWORD_SIZEOF},
    {"signed", Token::KEYWORD_SIGNED},
    {"short", Token::KEYWORD_SHORT},
    {"return", Token::KEYWORD_RETURN},
    {"restrict", Token::KEYWORD_RESTRICT},
    {"register", Token::KEYWORD_REGISTER},
    {"long", Token::KEYWORD_LONG},
    {"int", Token::KEYWORD_INT},
    {"inline", Token::KEYWORD_INLINE},
    {"if", Token::KEYWORD_IF},
    {"goto", Token::KEYWORD_GOTO},
    {"for", Token::KEYWORD_FOR},
    {"float", Token::KEYWORD_FLOAT},
    {"extern", Token::KEYWORD_EXTERN},
    {"enum", Token::KEYWORD_ENUM},
    {"else", Token::KEYWORD_ELSE},
    {"double", Token::KEYWORD_DOUBLE},
    {"do", Token::KEYWORD_DO},
    {"default", Token::KEYWORD_DEFAULT},
    {"continue", Token::KEYWORD_CONTINUE},
    {"const", Token::KEYWORD_CONST},
    {"char", Token::KEYWORD_CHAR},
    {"case", Token::KEYWORD_CASE},
    {"break", Token::KEYWORD_BREAK},
    {"auto", Token::KEYWORD_AUTO},
    {"_Bool", Token::KEYWORD__BOOL},
};

void Lexer::eatToken() {
  while (getChar()) {
    Token::Kind kind = matchTable[currChar];
    switch (kind) {
    default:
      getPosToken(Token::INVALID);
      eatChar();
      return;
    case Token::LITERAL_STR:
      eatLiteralStr<Token::LITERAL_STR>();
      return;
    case Token::LITERAL_CHAR:
      eatLiteralStr<Token::LITERAL_CHAR>();
      return;
    case Token::IDENTIFIER:
    case Token::LITERAL_NUM:
      eatIdentifier(kind);
      probeKeyword();
      return;
    case Token::SPACE:
      dropChar();
      continue;
    case Token::PUNCT_PLUS:
      eatOperator<Token::PUNCT_PLUS, Token::PUNCT_PLUSPLUS,
                  Token::PUNCT_PLUSEQ>();
      return;
    case Token::PUNCT_MINUS:
      eatOperator<Token::PUNCT_MINUS, Token::PUNCT_MINUSMINUS,
                  Token::PUNCT_MINUSEQ>();
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
      eatOperator<Token::PUNCT_EQ, Token::PUNCT_EQEQ>();
      return;
    case Token::PUNCT_EXCLAMATION:
      eatOperator<Token::PUNCT_EXCLAMATION, Token::EMPTY,
                  Token::PUNCT_EXCLAMATIONEQ>();
      return;
    case Token::PUNCT_XOR:
      eatOperator<Token::PUNCT_XOR, Token::EMPTY, Token::PUNCT_XOREQ>();
      return;
    case Token::PUNCT_SLASH:
      eatOperator<Token::PUNCT_SLASH, Token::EMPTY, Token::PUNCT_SLASHEQ>();
      return;
    case Token::PUNCT_PERCENT:
      eatOperator<Token::PUNCT_PERCENT, Token::EMPTY, Token::PUNCT_PERCENTEQ>();
      return;
    case Token::PUNCT_LT:
      eatOperator<Token::PUNCT_LT, Token::PUNCT_LTLT, Token::PUNCT_LTEQ,
                  Token::PUNCT_LTLTEQ>();
      return;
    case Token::PUNCT_GT:
      eatOperator<Token::PUNCT_GT, Token::PUNCT_GTGT, Token::PUNCT_GTEQ,
                  Token::PUNCT_GTGTEQ>();
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
    case Token::PUNCT_DOT:
    case Token::PUNCT_TILDE:
      getPosToken(kind);
      eatChar();
      return;
    }
  }
  currTok = Token(Token::END);
}

void Lexer::eatIdentifier(Token::Kind kind) {
  getPosToken(kind);
  while (getChar()) {
    switch (matchTable[currChar]) {
    case Token::IDENTIFIER:
    case Token::LITERAL_NUM:
      eatChar();
      break;
    default:
      return;
    }
  }
}

void Lexer::probeKeyword() {
  if (!currTok.isValidString())
    return;
  if (auto i = keywords.find(std::string_view(currTok)); i != keywords.end()) {
    currTok.kind = i->second;
  }
}

void Lexer::getToken() {
  if (currTok.isEnd()) {
    return;
  }
  eatToken();
}

Token Lexer::peekToken() { return currTok; }

Token Lexer::nextToken() {
  Token tok = currTok;
  getToken();
  return tok;
}

bool Lexer::matchPeekToken(Token::Kind kind) {
  if (currTok.kind != kind)
    return false;

  return true;
}

bool Lexer::matchNextToken(Token::Kind kind) {
  if (currTok.kind != kind)
    return false;

  getToken();
  return true;
}

const char *Lexer::getPosPtr() { return &currPos[0]; }

void Lexer::getPosToken(Token::Kind kind) {
  currTok = Token(kind, getPosPtr());
}

bool Token::isEmpty() const { return kind == EMPTY; }

bool Token::isInvalid() const { return kind == INVALID; }

bool Token::isEnd() const { return kind == END; }

bool Token::isKeyword() const { return isKeyword(kind); }

bool Token::isPunctuator() const { return isPunctuator(kind); }

bool Token::isValidString() const { return text && textLen > 0; }

Token::Kind Lexer::peekTokenKind() { return currTok.kind; };
void Lexer::dropChar() { ++currPos; }

void Lexer::eatChar() {
  ++currPos;
  ++currTok.textLen;
}
void Lexer::dropToken() { getToken(); }

const char *Token::kindName(Kind kind) {
  switch (kind) {
  case END:
    return "EOF";
  case SPACE:
    return "whitespace";
  case COMMENT:
    return "comment";
  case IDENTIFIER:
    return "identifier";
  case LITERAL_NUM:
    return "number literal";
  case LITERAL_CHAR:
    return "char literal";
  case LITERAL_STR:
    return "string literal";
  case ESCAPE:
    return "\\";
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
  case KEYWORD__BOOL:
    return "_Bool";
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
  default:
    return "<illegal>";
  }
}
} // namespace c
