#include "c/ASTPrinter.h"
#include "c/AST.h"
#include "c/ASTVisitor.h"
#include "c/Type.h"

#include <unordered_set>

namespace c {

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

  void visitMemberAccess(MemberAccessAST &ast) {
    if (ast.getKind() == AST::ACCESS_MEMBER_DEREF) {
      std::cout << "MemberAccessDeref(";
    } else {
      std::cout << "MemberAccess(";
    }
    dispatch(*ast.child);
    std::cout << ",";
    std::cout << ast.ident;
    std::cout << ")";
  }

  void visitArrAccess(ArrAccessAST &ast) {
    std::cout << "ArrAccess(";
    dispatch(*ast.arr);
    std::cout << ",";
    dispatch(*ast.expr);
    std::cout << ")";
  }

  void visitFunctionCall(FunctionCallAST &ast) {
    std::cout << "FunctionCall(";
    dispatch(*ast.child);
    for (auto &arg : ast.args) {
      std::cout << ",";
      dispatch(*arg);
    }
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
    dispatch(ast.expr.get());
    std::cout << ")";
    dispatch(ast.st.get());
  }

  void visitForSt(ForStAST &ast) {
    std::cout << "for(";
    dispatch(ast.initClause.get());
    std::cout << "; ";
    dispatch(ast.exprCond.get());
    std::cout << "; ";
    dispatch(ast.exprIter.get());
    std::cout << ")";
    dispatch(ast.st.get());
  }

  void visitReturnSt(ReturnStAST &ast) {
    std::cout << "return";
    if (ast.expr) {
      std::cout << " ";
      dispatch(*ast.expr);
    }
  }

  void visitDeclarator(DeclaratorAST &ast) {
    std::cout << ast.ident << " is ";
    printType(ast.type);
  }

  void visitDeclaration(DeclarationAST &ast) {
    printSymbolKind(ast.symbolKind);
    std::cout << " ";
    bool first = true;
    for (auto &[d, initializer] : ast.declarators) {
      if (first) {
        first = false;
      } else {
        std::cout << ",\n";
      }
      dispatch(d);
      if (initializer) {
        std::cout << " = ";
        dispatch(*initializer);
      }
    }
  }

  void visitFunctionDefinition(FunctionDefinitionAST &ast) {
    printSymbolKind(ast.symbolKind);
    std::cout << " ";
    dispatch(ast.decl);
    dispatch(ast.st.get());
  }

  void visitTranslationUnit(TranslationUnitAST &ast) {
    for (auto &d : ast.declarations) {
      dispatch(d.get());
      std::cout << ";\n";
    }
  }

  void printSymbolKind(Symbol::Kind symbolKind) {
    switch (symbolKind) {
    case Symbol::EMPTY:
      std::cout << "default";
      break;
    case Symbol::TYPEDEF:
      std::cout << "typedef";
      break;
    case Symbol::EXTERN:
      std::cout << "extern";
      break;
    case Symbol::STATIC:
      std::cout << "static";
      break;
    case Symbol::AUTO:
      std::cout << "auto";
      break;
    case Symbol::REGISTER:
      std::cout << "register";
      break;
    }
  }

  void printQualifier(const QualifiedType::Qualifier &quali) {
    if (quali.isConst()) {
      std::cout << "const ";
    }
    if (quali.isRestrict()) {
      std::cout << " restrict ";
    }
    if (quali.isVolatile()) {
      std::cout << " volatile ";
    }
    if (quali.isAtomic()) {
      std::cout << "atomic ";
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
      printType(&as<DerivedType>(*type).getBaseType());
      break;
    case Type::FUNC: {
      std::cout << "function(";
      bool first = true;
      FuncType *p = static_cast<FuncType *>(type);
      for (auto &[ident, t] : p->getParams()) {
        if (!first) {
          std::cout << ", ";
        }
        first = false;
        printType(t);
        if (!ident.empty()) {
          std::cout << " " << ident;
        }
      }
      std::cout << ") returning ";
      printType(&p->getBaseType());
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
      printStructType(as<StructType>(*type));
      break;
    case Type::QUALIFIED: {
      auto &qTy = as<QualifiedType>(*type);
      printQualifier(qTy.getQualifier());
      printType(&qTy.getBaseType());
      break;
    }
    default:
      std::cout << "unnamed";
      break;
    }
  }

  void printStructType(StructType &type) {
    auto [_, succ] = seenTypes.insert(&type);
    if (!succ) {
      std::cout << "...";
      return;
    }
    std::cout << "struct {";
    for (auto &subType : type.members) {
      std::cout << "\n";
      printType(subType);
      std::cout << ";";
    }
    std::cout << "\n";
    std::cout << "}";
  }

private:
  std::unordered_set<Type *> seenTypes;
};
} // namespace

void PrintAST(AST &ast) { PrintASTVisitor().dispatch(ast); }
} // namespace c
