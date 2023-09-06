#pragma once

#include "frontend/Symbol.h"
#include "frontend/Type.h"
#include "ir/IR.h"
#include "support/RefCount.h"
#include <cassert>

class ExpressionSemantics {
public:
  enum Action {
    SSA_DOWNGRADE,
    CONV_LVALUE,
  };

  enum Result {
    SUCCESS,
    ERROR,
    ERROR_EXPECT_LVALUE,
    ERROR_EXPECT_RVALUE,
    ERROR_EXPECT_TYPE,
  };

  enum Category { LVALUE, RVALUE };

  class Handler {
  public:
    Result tryAction(Action action) {
      switch (action) {
      case SSA_DOWNGRADE:
        return semanticSSADowngrade();
      case CONV_LVALUE:
        return semanticConvLValue();
      }
    }

    void emitError(Result error) { semanticError(error); }

  private:
    virtual Result semanticConvLValue() = 0;
    virtual Result semanticSSADowngrade() = 0;
    virtual void semanticError(Result error) = 0;
  };

  ExpressionSemantics(Handler &handler) : handler(&handler) {}

  Category getCategory() { return category; }

  CountedPtr<Type> &getType() { return type; }

  void fromSymbol(Symbol &sym) {
    category = LVALUE;
    type = sym.getType();
  }

  void fromConstant(CountedPtr<Type> ty) {
    category = RVALUE;
    type = std::move(ty);
  }

  void deref() {
    expectRValue();
    expectTypeKind(Type::PTR);
    category = LVALUE;
    type = std::move(static_cast<PtrType *>(type.get())->getBaseType());
  }

  void addr() {
    expectLValue();
    tryAction(SSA_DOWNGRADE);
    category = RVALUE;
    type = make_counted<PtrType>(std::move(type));
  }

  void setCategory(Category c) { category = c; }

  void expectLValue() {
    if (category != LVALUE) {
      error(ERROR_EXPECT_LVALUE);
    }
  }

  void tryAction(Action action) {
    auto res = handler->tryAction(action);
    if (res != SUCCESS) {
      error(res);
    }
  }

  void tryAction(Action action, Result err) {
    if (handler->tryAction(action) != SUCCESS) {
      error(err);
    }
  }

  void error(Result err) { handler->emitError(err); }

  void expectRValue() {
    if (category != RVALUE) {
      tryAction(CONV_LVALUE, ERROR_EXPECT_RVALUE);
      category = RVALUE;
    }
  }

  void expectTypeKind(Type::Kind kind) {
    assert(type);
    if (type->getKind() != kind) {
      error(ERROR_EXPECT_TYPE);
    }
  }

private:
  CountedPtr<Type> type;
  Category category;
  Handler *handler;
};
