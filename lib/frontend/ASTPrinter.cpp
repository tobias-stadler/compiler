#include "frontend/ASTPrinter.h"

#include "frontend/ASTVisitor.h"
#include "frontend/Type.h"

namespace {

class PrintASTVisitor : public ASTVisitor<PrintASTVisitor> {
public:
  void visit(AST &ast) { std::cout << AST::kindName(ast.getKind()); }

  void visitVar(VarAST &ast) { std::cout << "Var(" << ast.ident << ")"; }

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
    std::cout << ")";
    dispatch(ast.getStatement());
    if (ast.hasElseStatement()) {
      std::cout << "\nelse";
      dispatch(ast.getElseStatement());
    }
  }

  void visitWhileSt(WhileStAST &ast) {
    std::cout << "while(";
    dispatch(ast.getExpression());
    std::cout << ")";
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
  }

  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    dispatch(ast.decl);
    for (auto [name, idx] :
         static_cast<FuncType *>(ast.decl.type.get())->getParamIndex()) {
      std::cout << "\n" << name << ": " << idx;
    }
    dispatch(ast.st.get());
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
      printType(static_cast<DerivedType *>(type)->getBaseType().get());
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
      printType(p->getBaseType().get());
      break;
    }
    case Type::VOID:
      std::cout << "void";
      break;
    case Type::BOOL:
      std::cout << "bool";
      break;
    case Type::SCHAR:
      std::cout << "signed char";
      break;
    case Type::SSHORT:
      std::cout << "signed short";
      break;
    case Type::SLONG:
      std::cout << "signed long";
      break;
    case Type::SLONGLONG:
      std::cout << "signed long long";
      break;
    case Type::UCHAR:
      std::cout << "unsigned char";
      break;
    case Type::USHORT:
      std::cout << "unsigned short";
      break;
    case Type::UINT:
      std::cout << "unsigned int";
      break;
    case Type::ULONG:
      std::cout << "unsigned long";
      break;
    case Type::ULONGLONG:
      std::cout << "unsigned long long";
      break;
    case Type::ARR:
      std::cout << "array";
      break;
    case Type::STRUCT:
      std::cout << "struct";
      break;
    default:
      std::cout << "unnamed";
      break;
    }
  }
};
} // namespace

void PrintAST(AST &ast) {
  PrintASTVisitor().dispatch(ast);
  std::cout << "\n";
}
