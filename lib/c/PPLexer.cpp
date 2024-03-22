#include "c/PPLexer.h"
#include <cassert>
#include <iostream>

#define PPLEXER_DEBUG

namespace c {

std::unordered_map<std::string_view, Token::Kind> PPLexer::keywords = {
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
    {"_Alignof", Token::KEYWORD_ALIGNOF},
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
    {"_Bool", Token::KEYWORD_BOOL},
};

std::unordered_map<std::string_view, PPDirective> PPLexer::ppKeywords = {
    {"if", PPDirective::IF},           {"ifdef", PPDirective::IFDEF},
    {"ifndef", PPDirective::IFNDEF},   {"elif", PPDirective::ELIF},
    {"else", PPDirective::ELSE},       {"endif", PPDirective::ENDIF},
    {"include", PPDirective::INCLUDE}, {"define", PPDirective::DEFINE},
    {"undef", PPDirective::UNDEF},     {"line", PPDirective::LINE},
    {"error", PPDirective::ERROR},     {"pragma", PPDirective::PRAGMA},
};

void PPLexer::getToken() {
  do {
    do {
      handleAllPPDirectives();
      ctx.handleReplacement();
    } while (ctx.out.empty());
    currTok = ctx.out[0];
    ctx.out.clear();
  } while (currTok.isWhiteSpace());
  probeKeyword();
#ifdef PPLEXER_DEBUG
  std::cerr << "[Debug][PPLexer] " << currTok << "\n";
#endif
}
} // namespace c
