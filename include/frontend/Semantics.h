#pragma once

#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "ir/IR.h"
#include "support/RefCount.h"
#include <cassert>

class ExpressionSemantics {
public:
  enum Action {
    CONV_LVALUE,
    CONV_BOOL_CMPZERO,
  };

  enum Result {
    SUCCESS,
    ERROR,
    ERROR_EXPECT_LVALUE,
    ERROR_EXPECT_RVALUE,
    ERROR_EXPECT_TYPE,
  };

  enum Category { LVALUE_SYMBOL, LVALUE_MEM, RVALUE };

  class Handler {
  public:
    Result tryAction(Action action) {
      switch (action) {
      case CONV_LVALUE:
        return semanticConvLValue();
      case CONV_BOOL_CMPZERO:
        return semanticConvBool();
      }
    }

    void emitError(Result error) { semanticError(error); }

  private:
    virtual Result semanticConvLValue() = 0;
    virtual Result semanticConvBool() = 0;
    virtual void semanticError(Result error) = 0;
  };

  ExpressionSemantics(Handler &handler) : handler(&handler) {}

  Category getCategory() { return category; }

  CountedPtr<Type> &getType() { return type; }

  void fromSymbol(Symbol &sym) {
    category = LVALUE_SYMBOL;
    type = sym.getType();
  }

  void fromConstant(CountedPtr<Type> ty) {
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

  bool isLValue() {
    return category == LVALUE_SYMBOL || category == LVALUE_MEM;
  }
  bool isRValue() { return category == RVALUE; }

  void expectLValue() {
    if (!isLValue()) {
      error(ERROR_EXPECT_LVALUE);
    }
  }

  void tryAction(Action action) {
    auto res = handler->tryAction(action);
    if (res != SUCCESS) {
      error(res);
    }
  }

  void error(Result err) { handler->emitError(err); }

  void expectRValue() {
    if (category == RVALUE) {
      return;
    }
    if (isLValue()) {
      tryAction(CONV_LVALUE);
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
      if (Type::isInteger(type->getKind())) {
        tryAction(CONV_BOOL_CMPZERO);
        type = BasicType::create(Type::BOOL);
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
