#pragma once
#include "frontend/AST.h"
#include "frontend/Lexer.h"
#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include <cassert>
#include <string_view>

class Parser {
public:
  Parser(Lexer &lex, SymbolTable &sym) : lex(&lex), sym(&sym) {}

public:
  ASTPtrResult parseLiteralNum();

  ASTError error(std::string_view str, Token tok = Token::EMPTY);
  ASTError nop() { return ASTError(ASTError::NOP); };

  bool nextIsAbstractDeclarator();

  bool nextIsDeclarationList();

  ASTPtrResult parseStatement();

  ASTPtrResult parseExpression(int prec = 1);

  ASTPtrResult parsePrimary();

  ASTPtrResult parseDeclaration();

  ASTResult<DeclaratorAST> parseDeclarator(bool abstract);
  ASTResult<DeclaratorAST> parseDirectDeclarator(bool abstract);
  ASTResult<DeclaratorAST> parseSingleDirectDeclarator(bool abstract,
                                                       bool first);

  ASTPtrResult parseBlockItem();
  ASTPtrResult parseCompoundStatement();
  ASTResult<std::unique_ptr<TranslationUnitAST>> parseTranslationUnit();

  template <bool enableType = true, bool enableQuali = true,
            bool enableStorage = true>
  ASTResult<DeclSpec> parseDeclSpec() {
    bool isNop = true;
    Type::Kind typeKind = Type::EMPTY;
    Type::Qualifier qualifier = Type::Qualifier();
    Symbol::Kind storageKind = Symbol::EMPTY;
    std::array<unsigned, 100> keys = {};
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
      ++keys[kind];
      lex->dropToken();
      isNop = false;
    }
  done:
    if (isNop) {
      return nop();
    }
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
      if (totalStorage == 0) {
        storageKind = Symbol::EMPTY;
      } else if (totalStorage == 1) {
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
      } else {
        return error("Multiple storage specifiers not allowed");
      }
    }
    if constexpr (enableQuali) {
      qualifier = Type::Qualifier(keys[Token::KEYWORD_CONST] > 0,
                                  keys[Token::KEYWORD_RESTRICT] > 0,
                                  keys[Token::KEYWORD_VOLATILE] > 0);
    }
    return DeclSpec(storageKind, typeKind, qualifier);
  }

private:
  Lexer *lex;
  SymbolTable *sym;
};
