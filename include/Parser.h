#pragma once
#include <Lexer.h>
#include <array>
#include <cassert>
#include <charconv>
#include <cstdint>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <shared_mutex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

class AST {
public:
  enum Kind {
    EMPTY,
    MUL,
    ADD,
    SUB,
    DIV,
    ASSIGN,
    EQUAL,
    BIT_AND,
    BIT_OR,
    LOG_AND,
    LOG_OR,
    VAR,
    NUM,
    ST_COMPOUND,
    ST_IF,
    ST_WHILE,
  };
  Kind kind;

  static constexpr const char *kindName(Kind kind) {
    switch (kind) {
    case MUL:
      return "Mul";
    case ADD:
      return "Add";
    case SUB:
      return "Sub";
    case VAR:
      return "Var";
    case NUM:
      return "Num";
    default:
      return "Unnamed";
    }
  }

  AST(Kind kind) : kind(kind) {}
  virtual ~AST() = default;
};

template <unsigned N> class ArrAST : public AST {
public:
  std::array<AST *, N> children;

  ArrAST(Kind kind, std::array<AST *, N> children = {})
      : AST(kind), children(std::move(children)) {}

  ArrAST(ArrAST &&o) noexcept { *this = std::move(o); }

  ArrAST &operator=(ArrAST &&o) noexcept {
    std::swap(children, o.children);
    kind = std::exchange(o.kind, EMPTY);
    return *this;
  }

  AST &getChild(unsigned n) {
    assert(children[n] && "Expected child to be non-null");
    return *children[n];
  }

  ~ArrAST() {
    for (AST *child : children) {
      delete child;
    }
  }
};

class VecAST : public AST {
public:
  std::vector<AST *> children;

  VecAST(Kind kind) : AST(kind) {}
  VecAST(Kind kind, std::vector<AST *> children)
      : AST(kind), children(std::move(children)) {}

  VecAST(VecAST &&o) noexcept : AST(EMPTY) { *this = std::move(o); }

  VecAST &operator=(VecAST &&o) noexcept {
    std::swap(children, o.children);
    kind = std::exchange(o.kind, EMPTY);
    return *this;
  }

  ~VecAST() {
    for (AST *child : children) {
      delete child;
    }
  }
};

class UnopAST : public ArrAST<1> {
public:
  UnopAST(Kind kind, AST *child) : ArrAST(kind, {child}) {}

  AST &getSubExpression() { return getChild(0); }
};

class BinopAST : public ArrAST<2> {
public:
  BinopAST(Kind kind, AST *lhs, AST *rhs) : ArrAST(kind, {lhs, rhs}) {}

  AST &getLHS() { return getChild(0); }
  AST &getRHS() { return getChild(1); }
};

class NumAST : public AST {
public:
  int64_t num;
  NumAST(uint64_t num) : AST(NUM), num(num) {}
};

class VarAST : public AST {
public:
  std::string_view varName;
  VarAST(std::string_view varName) : AST(VAR), varName(varName) {}
};

class CompoundStAST : public VecAST {
public:
  CompoundStAST() : VecAST(ST_COMPOUND) {}
};

class IfStAST : public ArrAST<3> {
public:
  IfStAST(AST *expr, AST *st, AST *stElse = nullptr)
      : ArrAST(ST_IF, {expr, st, stElse}) {}

  AST &getExpression() { return getChild(0); }
  AST &getStatement() { return getChild(1); }

  bool hasElse() { return children[2]; }

  AST &getElseStatement() { return getChild(2); }
};

class WhileStAST : public ArrAST<2> {
public:
  WhileStAST(AST *expr, AST *st) : ArrAST(ST_WHILE, {expr, st}) {}

  AST &getExpression() { return getChild(0); }
  AST &getStatement() { return getChild(1); }
};

class ParserException : public std::runtime_error {
public:
  ParserException(const std::string &arg)
      : std::runtime_error("Parser: " + arg) {}
};

#define VISIT_DELEGATE(AstTy)                                                  \
  impl()->visit##AstTy(*static_cast<AstTy##AST *>(ast));

template <class D> class ASTVisitor {
public:
  void run(AST *ast) { dispatch(ast); }

protected:
  void dispatch(AST &ast) { dispatch(&ast); }

  void dispatch(AST *ast) {
    if (!ast)
      return;
    switch (ast->kind) {
    case AST::EMPTY:
      break;
    default:
      impl()->visit(*ast);
      break;
    case AST::MUL:
    case AST::ADD:
    case AST::SUB:
    case AST::DIV:
    case AST::ASSIGN:
    case AST::EQUAL:
    case AST::BIT_AND:
    case AST::BIT_OR:
    case AST::LOG_AND:
    case AST::LOG_OR:
      VISIT_DELEGATE(Binop);
      break;
    case AST::VAR:
      VISIT_DELEGATE(Var);
      break;
    case AST::NUM:
      VISIT_DELEGATE(Num);
      break;
    case AST::ST_IF:
      VISIT_DELEGATE(IfSt);
      break;
    case AST::ST_COMPOUND:
      VISIT_DELEGATE(CompoundSt);
      break;
    case AST::ST_WHILE:
      VISIT_DELEGATE(WhileSt);
      break;
    }
  }

  template <int N> void dispatchChildren(ArrAST<N> &ast) {
    for (AST *child : ast.children) {
      dispatch(child);
    }
  }

  D *impl() { return static_cast<D *>(this); }

  void dispatchChildren(VecAST &ast) {
    for (AST *child : ast.children) {
      dispatch(child);
    }
  }

  void visit(AST &ast) {}
  void visitNum(NumAST &ast) { impl()->visit(ast); }
  void visitVar(VarAST &ast) { impl()->visit(ast); }
  void visitBinop(BinopAST &ast) { impl()->visit(ast); }
  void visitCompoundSt(CompoundStAST &ast) { impl()->visit(ast); }
  void visitIfSt(IfStAST &ast) { impl()->visit(ast); }
  void visitWhileSt(WhileStAST &ast) { impl()->visit(ast); }
};

class PrintASTVisitor : public ASTVisitor<PrintASTVisitor> {
public:
  void run(AST *ast) {
    dispatch(ast);
    std::cout << "\n";
  }

  void visit(AST &ast) { std::cout << AST::kindName(ast.kind); }

  void visitVar(VarAST &ast) { std::cout << "Var(" << ast.varName << ")"; }

  void visitNum(NumAST &ast) { std::cout << "Num(" << ast.num << ")"; }

  void visitBinop(BinopAST &ast) {
    std::cout << "Binop(" << AST::kindName(ast.kind);
    for (AST *child : ast.children) {
      std::cout << ",";
      dispatch(child);
    }
    std::cout << ")";
  }

  void visitCompoundSt(CompoundStAST &ast) {
    std::cout << "{\n";
    for (AST *child : ast.children) {
      dispatch(child);
      std::cout << ";\n";
    }
    std::cout << "}";
  }

  void visitIfSt(IfStAST &ast) {
    std::cout << "if(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
    if (ast.hasElse()) {
      std::cout << "\nelse\n";
      dispatch(ast.getElseStatement());
    }
  }

  void visistWhileSt(WhileStAST &ast) {
    std::cout << "while(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
  }
};

class Parser {
public:
  Parser(Lexer *lex) : lex(lex) { assert(lex); }

public:
  static constexpr AST::Kind tokenBinopKind(Token::Kind kind) {
    switch (kind) {
    default:
      return AST::EMPTY;
    case Token::PUNCT_STAR:
      return AST::MUL;
    case Token::PUNCT_PLUS:
      return AST::ADD;
    case Token::PUNCT_MINUS:
      return AST::SUB;
    }
  }

  static constexpr int tokenPrecedence(Token::Kind kind) {
    switch (kind) {
    case Token::PUNCT_STAR:
      return 3;
    case Token::PUNCT_PLUS:
    case Token::PUNCT_MINUS:
      return 2;
    default:
      return -1;
    }
  }

  static constexpr bool precedenceRightAssoc(int prec) {
    switch (prec) {
    default:
      return false;
    }
  }

  Lexer *lex;

  AST *parseLiteralNum() {
    Token tok = lex->nextToken();
    if (tok.kind != Token::LITERAL_NUM) {
      return error("Expected literal num", tok);
    }
    std::string_view str(tok);
    int64_t num;
    auto res = std::from_chars(str.data(), str.data() + str.size(), num);
    if (res.ec != std::errc{}) {
      return error("Invalid num", tok);
    }
    return new NumAST(num);
  }

  AST *error(std::string_view str, Token tok = Token::EMPTY) {
    std::cerr << "[Error][Parser] " << str;
    if (!tok.isEmpty()) {
      std::cerr << "; Exceptional Token: " << tok;
    }
    std::cerr << '\n';
    return nullptr;
  }

  AST *parseStatement() {
    switch (lex->peekTokenKind()) {
    case Token::PUNCT_CURLYO: {
      lex->dropToken();
      auto *st = new CompoundStAST();
      for (AST *subSt; (subSt = parseStatement());) {
        st->children.push_back(subSt);
      }
      if (!lex->matchNextToken(Token::PUNCT_CURLYC)) {
        delete st;
        return error("Expected closing curly after compund statement");
      }
      return st;
    }
    case Token::KEYWORD_IF: {
      lex->dropToken();
      if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
        return error("Expected opening paren after if keyword");
      }
      auto *expr = parseExpression();
      if (!expr) {
        return error("Expected expression after if keyword");
      }
      if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
        delete expr;
        return error("Expected closing paren after if expression");
      }
      auto *st = parseStatement();
      if (!st) {
        delete expr;
        return error("Expected statment after if");
      }
      AST *stElse = nullptr;
      if (lex->matchNextToken(Token::KEYWORD_ELSE) &&
          !(stElse = parseStatement())) {
        delete expr;
        delete st;
        return error("Expected statement after else");
      }
      return new IfStAST(expr, st, stElse);
    }
    case Token::KEYWORD_WHILE: {
      lex->dropToken();
      if (!lex->matchNextToken(Token::PUNCT_PARENO)) {
        return error("Expected opening paren after while keyword");
      }
      auto *expr = parseExpression();
      if (!expr) {
        return error("Expected expression after while keyword");
      }
      if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
        delete expr;
        return error("Expected closing paren after while expression");
      }
      auto *st = parseStatement();
      if (!st) {
        delete expr;
        return error("Expected statment after while");
      }
      return new WhileStAST(expr, st);
    }
    default: {
      auto *st = parseExpression();
      if (!st) {
        return nullptr;
      }
      if (!lex->matchNextToken(Token::PUNCT_SEMICOLON)) {
        delete st;
        return error("Expected semicolon after expression statement");
      }
      return st;
    }
    }
  }

  AST *parseExpression(int prec = 0) {
    AST *lhs = parsePrimary();
    while (true) {
      Token::Kind tokKind = lex->peekTokenKind();
      int tokPrec = tokenPrecedence(tokKind);
      if (tokPrec < prec) {
        return lhs;
      }
      lex->dropToken();
      AST *rhs = parseExpression(precedenceRightAssoc(tokPrec) ? tokPrec
                                                               : tokPrec + 1);
      lhs = new BinopAST(tokenBinopKind(tokKind), lhs, rhs);
    }
  }

  AST *parseType() { return nullptr; }

  AST *parsePrimary() {
    if (lex->matchNextToken(Token::PUNCT_PARENO)) {
      AST *ex = parseExpression();
      if (!lex->matchNextToken(Token::PUNCT_PARENC)) {
        delete ex;
        return error("Expected closing paren", lex->peekToken());
      }
      return ex;
    }
    if (lex->matchPeekToken(Token::IDENTIFIER)) {
      return new VarAST(std::string_view(lex->nextToken()));
    }
    if (lex->matchPeekToken(Token::LITERAL_NUM)) {
      return parseLiteralNum();
    }
    return nullptr;
  }
};
