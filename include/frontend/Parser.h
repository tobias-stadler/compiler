#pragma once
#include "frontend/Type.h"
#include <array>
#include <cassert>
#include <charconv>
#include <cstdint>
#include <frontend/AST.h>
#include <frontend/Lexer.h>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <shared_mutex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

class Parser {
public:
  Parser(Lexer *lex);

public:
  static constexpr AST::Kind tokenBinopKind(Token::Kind kind) {
    switch (kind) {
    default:
      return AST::EMPTY;
    case Token::PUNCT_EQ:
      return AST::ASSIGN;
    case Token::PUNCT_PLUSEQ:
      return AST::ASSIGN_ADD;
    case Token::PUNCT_MINUSEQ:
      return AST::ASSIGN_SUB;
    case Token::PUNCT_STAREQ:
      return AST::ASSIGN_MUL;
    case Token::PUNCT_SLASHEQ:
      return AST::ASSIGN_DIV;
    case Token::PUNCT_PERCENTEQ:
      return AST::ASSIGN_MOD;
    case Token::PUNCT_ANDEQ:
      return AST::ASSIGN_AND;
    case Token::PUNCT_OREQ:
      return AST::ASSIGN_OR;
    case Token::PUNCT_XOREQ:
      return AST::ASSIGN_XOR;
    case Token::PUNCT_LTLTEQ:
      return AST::ASSIGN_LSHIFT;
    case Token::PUNCT_GTGTEQ:
      return AST::ASSIGN_RSHIFT;
    case Token::PUNCT_PLUS:
      return AST::ADD;
    case Token::PUNCT_MINUS:
      return AST::SUB;
    case Token::PUNCT_STAR:
      return AST::MUL;
    case Token::PUNCT_SLASH:
      return AST::DIV;
    case Token::PUNCT_PERCENT:
      return AST::MOD;
    case Token::PUNCT_AND:
      return AST::BIT_AND;
    case Token::PUNCT_OR:
      return AST::BIT_OR;
    case Token::PUNCT_XOR:
      return AST::BIT_XOR;
    case Token::PUNCT_LTLT:
      return AST::LSHIFT;
    case Token::PUNCT_GTGT:
      return AST::RSHIFT;
    case Token::PUNCT_ANDAND:
      return AST::LOG_AND;
    case Token::PUNCT_OROR:
      return AST::LOG_OR;
    case Token::PUNCT_EQEQ:
      return AST::EQ;
    case Token::PUNCT_EXCLAMATIONEQ:
      return AST::NEQ;
    case Token::PUNCT_LT:
      return AST::LT;
    case Token::PUNCT_GT:
      return AST::GT;
    case Token::PUNCT_LTEQ:
      return AST::LTE;
    case Token::PUNCT_GTEQ:
      return AST::GTE;
    }
  }

  static constexpr AST::Kind tokenUnopKind(Token::Kind kind) {
    switch (kind) {
    default:
      return AST::EMPTY;
    case Token::PUNCT_STAR:
      return AST::DEREF;
    case Token::PUNCT_AND:
      return AST::ADDR;
    case Token::PUNCT_EXCLAMATION:
      return AST::LOG_NOT;
    case Token::PUNCT_TILDE:
      return AST::BIT_NOT;
    case Token::PUNCT_PLUSPLUS:
      return AST::INC_PRE;
    case Token::PUNCT_MINUSMINUS:
      return AST::DEC_PRE;
    }
  }

  static constexpr int binopPrecedence(AST::Kind kind) {
    switch (kind) {
    case AST::MUL:
    case AST::DIV:
      return 12;
    case AST::ADD:
    case AST::SUB:
      return 11;
    case AST::LSHIFT:
    case AST::RSHIFT:
      return 10;
    case AST::LT:
    case AST::GT:
    case AST::LTE:
    case AST::GTE:
      return 9;
    case AST::EQ:
    case AST::NEQ:
      return 8;
    case AST::BIT_AND:
      return 7;
    case AST::BIT_XOR:
      return 6;
    case AST::BIT_OR:
      return 5;
    case AST::LOG_AND:
      return 4;
    case AST::LOG_OR:
      return 3;
    case AST::ASSIGN:
    case AST::ASSIGN_ADD:
    case AST::ASSIGN_SUB:
    case AST::ASSIGN_MUL:
    case AST::ASSIGN_DIV:
    case AST::ASSIGN_MOD:
    case AST::ASSIGN_LSHIFT:
    case AST::ASSIGN_RSHIFT:
    case AST::ASSIGN_AND:
    case AST::ASSIGN_OR:
    case AST::ASSIGN_XOR:
      return 1;
    default:
      return -1;
    }
  }

  static constexpr bool precedenceRightAssoc(int prec) {
    switch (prec) {
    default:
      return false;
    case 1:
      return true;
    }
  }

  Lexer *lex;

  AST *parseLiteralNum();

  AST *error(std::string_view str, Token tok = Token::EMPTY);

  AST *parseStatement();

  AST *parseExpression(int prec = 1);

  AST *parsePrimary();

  AST *parseDeclaration();

  AST *parseDeclarator();

  template <bool enableType = true, bool enableQuali = true,
            bool enableStorage = true>
  AST *parseDeclSpec() {
    Type::Kind typeKind = Type::EMPTY;
    Type::Qualifier qualifier = Type::Qualifier();
    Symbol::Kind storageKind = Symbol::EMPTY;
    std::array<unsigned, Token::NUM_KEYWORDS> keys = {};
    unsigned totalType = 0, totalStorage = 0;
    while (true) {
      Token::Kind kind = lex->peekTokenKind();
      switch (kind) {
      default:
        goto done;
      case Token::KEYWORD_VOID:
      case Token::KEYWORD_CHAR:
      case Token::KEYWORD_SHORT:
      case Token::KEYWORD_INT:
      case Token::KEYWORD_LONG:
      case Token::KEYWORD_UNSIGNED:
      case Token::KEYWORD_SIGNED:
        if constexpr (enableType) {
          ++totalType;
          break;
        } else {
          goto done;
        }
      case Token::KEYWORD_CONST:
      case Token::KEYWORD_RESTRICT:
      case Token::KEYWORD_VOLATILE:
        if constexpr (enableQuali) {
          break;
        } else {
          goto done;
        }
      case Token::KEYWORD_TYPEDEF:
      case Token::KEYWORD_EXTERN:
      case Token::KEYWORD_STATIC:
      case Token::KEYWORD_AUTO:
      case Token::KEYWORD_REGISTER:
        if constexpr (enableStorage) {
          ++totalStorage;
          break;
        } else {
          goto done;
        }
      }
      ++keys[kind - Token::KEYWORD_START - 1];
      lex->dropToken();
    }
  done:
    if constexpr (enableType) {
      if (totalType == 1) {
        if (keys[Token::KEYWORD_VOID] == 1) {
          typeKind = Type::VOID;
        } else if (keys[Token::KEYWORD_CHAR] == 1) {
          typeKind = Type::SCHAR;
        } else if (keys[Token::KEYWORD_SHORT] == 1) {
          typeKind = Type::SSHORT;
        } else if (keys[Token::KEYWORD_INT] == 1) {
          typeKind = Type::SINT;
        } else if (keys[Token::KEYWORD_LONG] == 1) {
          typeKind = Type::SLONG;
        } else {
          return error("Invalid 1 type specifier set");
        }
      } else {
        return error("Invalid type specifier set");
      }
    }
    if constexpr (enableStorage) {
      if (totalStorage != 1) {
        return error("Multiple storage specifiers not allowed");
      }
      if (keys[Token::KEYWORD_AUTO] == 1) {
        storageKind = Symbol::AUTO;
      } else if (keys[Token::KEYWORD_STATIC] == 1) {
        storageKind = Symbol::STATIC;
      } else if (keys[Token::KEYWORD_TYPEDEF] == 1) {
        storageKind = Symbol::TYPE;
      } else if (keys[Token::KEYWORD_REGISTER] == 1) {
        storageKind = Symbol::REGISTER;
      } else {
        return error("Invalid storage specifier ");
      }
    }
    if constexpr (enableQuali) {
      qualifier = Type::Qualifier(keys[Token::KEYWORD_CONST] > 0,
                                  keys[Token::KEYWORD_RESTRICT] > 0,
                                  keys[Token::KEYWORD_VOLATILE] > 0);
    }
    return new DeclSpecAST(storageKind, typeKind, qualifier);
  }
};
