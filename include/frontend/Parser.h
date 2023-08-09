#pragma once
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

  AST *parseTypeSpec();
};
