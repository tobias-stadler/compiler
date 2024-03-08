#include "c/ASTPrinter.h"
#include "c/AST.h"
#include "c/ASTVisitor.h"
#include "c/Type.h"

#include <unordered_map>

namespace c {

namespace {

class PrintASTVisitor : public ASTVisitor<PrintASTVisitor> {
public:
  void visitNullptr() { std::cout << "NULL"; }

  void visit(AST &ast) { std::cout << AST::kindName(ast.getKind()); }

  void visitVar(VarAST &ast) { std::cout << "Var(" << ast.ident << ")"; }

  void visitType(TypeAST &ast) {
    std::cout << "Type(";
    printType(ast.type);
    std::cout << ")";
  }

  void visitIntConst(IntConstAST &ast) {
    std::cout << "IntConst(";
    printType(ast.type);
    std::cout << ",";
    if (ast.type && Type::isSigned(ast.type->getKind())) {
      std::cout << ast.num.getMWordSignedSext();
    } else {
      std::cout << ast.num.getMWordSext();
    }
    std::cout << ")";
  }

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
    std::cout << "MemberAccess(";
    dispatch(ast.child.get());
    std::cout << ",";
    std::cout << ast.ident;
    std::cout << ")";
  }

  void visitArrAccess(ArrAccessAST &ast) {
    std::cout << "ArrAccess(";
    dispatch(ast.arr.get());
    std::cout << ",";
    dispatch(ast.expr.get());
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

  void visitCast(CastAST &ast) {
    std::cout << "Cast(";
    dispatch(*ast.child);
    std::cout << ",";
    printType(ast.type);
    std::cout << ")";
  }

  void visitLabelSt(LabelStAST &ast) {
    std::cout << AST::kindName(ast.getKind());
    std::cout << " " << ast.ident;
    dispatch(ast.expr.get());
    std::cout << ": ";
    dispatch(ast.st.get());
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

  void visitTernary(TernaryAST &ast) {
    std::cout << "Ternary(";
    dispatch(ast.getCondition());
    std::cout << "?";
    dispatch(ast.getLHS());
    std::cout << ":";
    dispatch(ast.getRHS());
    std::cout << ")";
  }

  void visitWhileSt(WhileStAST &ast) {
    std::cout << AST::kindName(ast.getKind()) << "(";
    dispatch(ast.expr.get());
    std::cout << ")";
    dispatch(ast.st.get());
  }

  void visitSwitchSt(SwitchStAST &ast) {
    std::cout << "switch(";
    dispatch(ast.expr.get());
    std::cout << ")";
    dispatch(ast.st.get());
  }

  void visitGotoSt(GotoStAST &ast) { std::cout << "goto " << ast.ident; }

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

  void visitInitializerList(InitializerListAST &ast) {
    std::cout << "{";
    for (auto &e : ast.entries) {
      dispatch(e.designation.get());
      std::cout << " = ";
      dispatch(e.initializer.get());
      std::cout << ", ";
    }
    std::cout << "}";
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
    case Symbol::CONSTEXPR:
      std::cout << "constexpr";
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
    case Type::UNION:
      std::cout << "union ";
      [[fallthrough]];
    case Type::STRUCT:
      printStructType(as<StructType>(*type));
      break;
    case Type::QUALIFIED: {
      auto &qTy = as<QualifiedType>(*type);
      printQualifier(qTy.getQualifier());
      printType(&qTy.getBaseType());
      break;
    }
    case Type::ENUM: {
      printEnumType(as<EnumType>(*type));
      break;
    }
    default:
      std::cout << "unknown";
      break;
    }
  }

  void printSymbol(Symbol &sym) {
    printSymbolKind(sym.getKind());
    std::cout << " symbol ";
    std::cout << sym.getName();
    std::cout << " is ";
    printType(sym.type);
    if (sym.ast) {
      std::cout << " = ";
      dispatch(*sym.ast);
    }
  }

  std::pair<size_t, bool> declareType(Type &type) {
    auto [it, succ] = seenTypes.insert({&type, 0});
    if (!succ) {
      return {it->second, false};
    }
    it->second = seenTypesIdx++;
    return {it->second, true};
  }

  void printEnumType(EnumType &type) {
    auto [id, succ] = declareType(type);
    std::cout << "enum#" << id;
    if (!succ) {
      return;
    }
    std::cout << " {\n";
    for (auto *s : type.members) {
      printSymbol(*s);
      std::cout << ",\n";
    }
    std::cout << "}";
  }

  void printStructType(StructType &type) {
    auto [id, succ] = declareType(type);
    std::cout << "struct#" << id;
    if (!succ) {
      return;
    }
    std::cout << " {";
    for (auto &subType : type.members) {
      std::cout << "\n";
      printType(subType);
      std::cout << ";";
    }
    std::cout << "\n";
    std::cout << "}";
  }

private:
  std::unordered_map<Type *, size_t> seenTypes;
  size_t seenTypesIdx = 0;
};
} // namespace

void PrintAST(AST &ast) { PrintASTVisitor().dispatch(ast); }
} // namespace c
