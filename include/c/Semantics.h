#pragma once

#include "c/AST.h"
#include "c/Symbol.h"
#include "c/Type.h"
#include <cassert>

namespace c {

class ExpressionSemantics {
public:
  enum Result {
    SUCCESS,
    ERROR,
    ERROR_EXPECT_LVALUE,
    ERROR_EXPECT_RVALUE,
    ERROR_EXPECT_TYPE,
  };

  enum Category { LVALUE_SYMBOL, LVALUE_MEM, RVALUE };

  static constexpr unsigned intConvRank(Type::Kind kind) {
    switch (kind) {
    case Type::BOOL:
      return 1;
    case Type::SCHAR:
    case Type::UCHAR:
      return 2;
    case Type::SSHORT:
    case Type::USHORT:
      return 3;
    case Type::SINT:
    case Type::UINT:
      return 4;
    case Type::SLONG:
    case Type::ULONG:
      return 5;
    case Type::SLONGLONG:
    case Type::ULONGLONG:
      return 6;
    default:
      return 0;
    }
  }

  static constexpr Type::Kind intPromotion(Type::Kind kind) {
    unsigned kindRank = intConvRank(kind);
    if (kindRank == 0) {
      return Type::EMPTY;
    }
    if (kindRank > intConvRank(Type::SINT)) {
      return kind;
    }
    switch (kind) {
    default:
      __builtin_unreachable();
    case Type::BOOL:
    case Type::SCHAR:
    case Type::UCHAR:
    case Type::SSHORT:
    case Type::USHORT:
    case Type::SINT:
      return Type::SINT;
    case Type::UINT:
      return Type::UINT;
    }
  }

  static constexpr Type::Kind usualArithConv(Type::Kind lhs, Type::Kind rhs) {
    if (!(Type::isInteger(lhs) && Type::isInteger(rhs))) {
      return Type::EMPTY;
    }
    lhs = intPromotion(lhs);
    rhs = intPromotion(rhs);
    if (lhs == rhs) {
      return lhs;
    }
    bool lhsSigned = Type::isSigned(lhs);
    bool rhsSigned = Type::isSigned(rhs);
    unsigned lhsRank = intConvRank(lhs);
    unsigned rhsRank = intConvRank(rhs);
    if (lhsSigned == rhsSigned) {
      if (lhsRank < rhsRank) {
        return rhs;
      } else {
        return lhs;
      }
    }
    Type::Kind signedKind = lhsSigned ? lhs : rhs;
    Type::Kind unsignedKind = lhsSigned ? rhs : lhs;
    unsigned signedRank = lhsSigned ? lhsRank : rhsRank;
    unsigned unsignedRank = lhsSigned ? rhsRank : lhsRank;
    if (unsignedRank >= signedRank) {
      return unsignedKind;
    }
    // TODO: Check if signed type can represent all values of unsigned type
    return signedKind;
  }

  class Handler {
    friend class ExpressionSemantics;

  protected:
    virtual Result semanticConvLValue() = 0;
    virtual Result semanticConvBool() = 0;
    virtual Result semanticConvInt(Type::Kind expectedKind) = 0;
    virtual void semanticError(Result error) = 0;

    Handler(ASTContext &ctx) : ctx(ctx) {}

    ASTContext &ctx;
  };

  ExpressionSemantics(Handler &handler) : handler(&handler) {}

  Category getCategory() { return category; }

  Type &getType() { return *type; }

  Type::Kind getTypeKind() { return type->getKind(); }

  void fromSymbol(Symbol &sym) {
    category = LVALUE_SYMBOL;
    type = &sym.getType();
  }

  void fromType(Type &ty) {
    category = RVALUE;
    type = &ty;
  }

  void baseType() { type = &as<DerivedType>(*type).getBaseType(); }

  void ptrType() { type = &handler->ctx.make_type<PtrType>(type); }

  void expectPromotedInt() {
    expectRValueImplicitConv();
    expectTypeKindImplicitConv(intPromotion(type->getKind()));
  }

  void setCategory(Category c) { category = c; }
  void setType(Type &ty) { type = &ty; }

  bool isLValue() {
    return category == LVALUE_SYMBOL || category == LVALUE_MEM;
  }

  bool isRValue() { return category == RVALUE; }

  void error(Result err) { handler->semanticError(err); }

  void expectCategory(Category cat) {
    if (category != cat) {
      error(ERROR);
    }
  }

  void expectLValue() {
    if (!isLValue()) {
      error(ERROR_EXPECT_LVALUE);
    }
  }

  void expectRValueImplicitConv() {
    if (category == RVALUE) {
      return;
    }
    if (isLValue() && handler->semanticConvLValue() == SUCCESS) {
      category = RVALUE;
      return;
    }
    error(ERROR_EXPECT_RVALUE);
  }

  bool implicitConv(Type::Kind expectedKind) {
    assert(type);
    if (expectedKind == Type::BOOL) {
      if (Type::isInteger(type->getKind()) &&
          handler->semanticConvBool() == SUCCESS) {
        type = &handler->ctx.make_type<BasicType>(Type::BOOL);
        return true;
      }
    } else if (Type::isInteger(expectedKind) &&
               Type::isInteger(type->getKind())) {
      if (handler->semanticConvInt(expectedKind) == SUCCESS) {
        type = &handler->ctx.make_type<BasicType>(expectedKind);
        return true;
      }
    }
    return false;
  }

  void expectTypeImplicitConv(Type &expectedType) {
    assert(type);
    if (expectedType == *type) {
      return;
    }
    if (implicitConv(expectedType.getKind())) {
      return;
    }
    error(ERROR_EXPECT_TYPE);
  }

  void expectTypeKindImplicitConv(Type::Kind expectedKind) {
    assert(type);
    if (type->getKind() == expectedKind) {
      return;
    }
    if (implicitConv(expectedKind)) {
      return;
    }
    error(ERROR_EXPECT_TYPE);
  }

  void expectTypeKind(Type::Kind expectedKind) {
    assert(type);
    if (type->getKind() == expectedKind) {
      return;
    }
    error(ERROR_EXPECT_TYPE);
  }

private:
  Type *type;
  Category category;
  Handler *handler;
};

} // namespace c
