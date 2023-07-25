#include <Lexer.h>

Token::Token(Kind kind, std::string_view text) : kind(kind), text(text){};

Lexer::Lexer(std::string_view input) : input(input) {

};

Token Lexer::nextToken(){
    return Token{Token::TOK_SEMICOLON, ""};
}
