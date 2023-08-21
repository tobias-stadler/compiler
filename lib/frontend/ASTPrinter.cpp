#include "frontend/ASTPrinter.h"

#include "frontend/ASTVisitor.h"

namespace {

class PrintASTVisitor : public ASTVisitor<PrintASTVisitor> {
public:
  void run(AST *ast) {
    dispatch(ast);
    std::cout << "\n";
  }

  void visit(AST &ast) { std::cout << AST::kindName(ast.getKind()); }

  void visitVar(VarAST &ast) { std::cout << "Var(" << ast.varName << ")"; }

  void visitNum(NumAST &ast) { std::cout << "Num(" << ast.num << ")"; }

  void visitUnop(UnopAST &ast) {
    std::cout << "Unop(" << AST::kindName(ast.getKind());
    std::cout << ",";
    dispatch(ast.getSubExpression());
    std::cout << ")";
  }
  void visitBinop(BinopAST &ast) {
    std::cout << "Binop(" << AST::kindName(ast.getKind());
    std::cout << ",";
    dispatch(ast.getLHS());
    std::cout << ",";
    dispatch(ast.getRHS());
    std::cout << ")";
  }

  void visitCompoundSt(CompoundStAST &ast) {
    std::cout << "\n{";
    for (auto &child : ast.children) {
      std::cout << "\n";
      dispatch(*child);
      std::cout << ";";
    }
    std::cout << "\n}";
  }

  void visitIfSt(IfStAST &ast) {
    std::cout << "if(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
    if (ast.hasElseStatement()) {
      std::cout << "\nelse\n";
      dispatch(ast.getElseStatement());
    }
  }

  void visitWhileSt(WhileStAST &ast) {
    std::cout << "while(";
    dispatch(ast.getExpression());
    std::cout << ")\n";
    dispatch(ast.getStatement());
  }

  void visitDeclarator(DeclaratorAST &ast) {
    std::cout << ast.ident << " is ";
    printType(ast.type.get());
  }

  void visitDeclaration(DeclarationAST &ast) {
    bool first = true;
    for (auto &d : ast.declarators) {
      if (!first) {
        std::cout << ",\n";
      }
      first = false;
      dispatch(d);
    }
    if (ast.st) {
      dispatch(ast.st.get());
    }
  }

  void visitTranslationUnit(TranslationUnitAST &ast) {
    for (auto &d : ast.declarations) {
      dispatch(d.get());
    }
  }

  void printType(Type *type) {
    if (!type) {
      std::cout << "NULL";
      return;
    }
    switch (type->getKind()) {
    case Type::SINT:
      std::cout << "signed int";
      break;
    case Type::PTR:
      std::cout << "pointer to ";
      printType(&static_cast<DerivedType *>(type)->getBaseType());
      break;
    case Type::FUNC: {
      std::cout << "function(";
      bool first = true;
      FuncType *p = static_cast<FuncType *>(type);
      for (auto &t : p->getParamTypes()) {
        if (!first) {
          std::cout << ", ";
        }
        first = false;
        printType(t.get());
      }
      std::cout << ") returning ";
      printType(&p->getBaseType());
      break;
    }
    default:
      std::cout << "unnamed";
    }
  }
};
} // namespace

void PrintAST(AST *ast) { PrintASTVisitor().run(ast); }
