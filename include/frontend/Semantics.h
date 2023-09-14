#pragma once

#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "ir/IR.h"
#include "support/RefCount.h"
#include <algorithm>
#include <cassert>

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
  public:
    virtual Result semanticConvLValue() = 0;
    virtual Result semanticConvBool() = 0;
    virtual Result semanticConvInt(Type::Kind expectedKind) = 0;
    virtual void semanticError(Result error) = 0;
  };

  ExpressionSemantics(Handler &handler) : handler(&handler) {}

  Category getCategory() { return category; }

  CountedPtr<Type> &getType() { return type; }

  void fromSymbol(Symbol &sym) {
    category = LVALUE_SYMBOL;
    type = sym.getType();
  }

  void fromType(CountedPtr<Type> ty) {
    category = RVALUE;
    type = std::move(ty);
  }

  void deref() {
    expectRValue();
    expectTypeKind(Type::PTR);
    category = LVALUE_MEM;
    type = static_cast<PtrType *>(type.get())->getBaseType();
  }

  void addr() {
    expectLValue();
    category = RVALUE;
    type = make_counted<PtrType>(std::move(type));
  }

  void setCategory(Category c) { category = c; }
  void setType(CountedPtr<Type> ty) { type = std::move(ty); }

  bool isLValue() {
    return category == LVALUE_SYMBOL || category == LVALUE_MEM;
  }
  bool isRValue() { return category == RVALUE; }

  void error(Result err) { handler->semanticError(err); }

  void expectLValue() {
    if (!isLValue()) {
      error(ERROR_EXPECT_LVALUE);
    }
  }

  void expectRValue() {
    if (category == RVALUE) {
      return;
    }
    if (isLValue() && handler->semanticConvLValue() == SUCCESS) {
      category = RVALUE;
      return;
    }
    error(ERROR_EXPECT_RVALUE);
  }

  void expectTypeKind(Type::Kind expectedKind) {
    assert(type);
    if (type->getKind() == expectedKind) {
      return;
    }
    if (expectedKind == Type::BOOL) {
      if (Type::isInteger(type->getKind()) &&
          handler->semanticConvBool() == SUCCESS) {
        type = BasicType::create(Type::BOOL);
        return;
      }
    } else if (Type::isInteger(expectedKind) &&
               Type::isInteger(type->getKind())) {
      if (handler->semanticConvInt(expectedKind) == SUCCESS) {
        type = BasicType::create(expectedKind);
        return;
      }
    }
    error(ERROR_EXPECT_TYPE);
  }

private:
  CountedPtr<Type> type;
  Category category;
  Handler *handler;
};
