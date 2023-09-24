#pragma once

#include "ir/IR.h"
#include <cassert>
#include <unordered_map>
#include <utility>

template <typename PassT> class IRPass;
template <typename PassT> class IRPipeline;
template <typename PassT> class IRInfo;

using IRInfoID = const void *;

template <typename PassT> class IRPass {
public:
  virtual ~IRPass() {}
  virtual void run(PassT &obj, IRInfo<PassT> &info) = 0;
  virtual const char *name() = 0;
  virtual void advertise(IRInfo<PassT> &) {}
  virtual void invalidate(IRInfo<PassT> &) {}
};

enum class PassInvalidation {
  NONE,
  ALL,
};

template <typename PassT> class PassContext {
public:
  PassContext() {}
  PassContext(IRPass<PassT> *pass, PassT *passObj) : pass(pass), obj(passObj) {}
  IRPass<PassT> *pass = nullptr;
  PassT *obj = nullptr;
  bool invalidateAll = false;
};

template <typename PassT> class IRInfo {
  friend class IRPipeline<PassT>;

public:
  IRInfo() {}

  template <typename T> void advertise() {
    assert(advertiser.find(&T::ID) == advertiser.end() &&
           "Multiple passes advertise the same data.");
    advertiser.try_emplace(&T::ID, ctx.pass);
  }

  template <typename T> void publish(T &val) {
    data[&T::ID] = &val;
    publisher[&T::ID] = ctx.pass;
  }

  template <typename T> T &query() {
    auto it = data.find(&T::ID);
    void *dataPtr = nullptr;
    if (it == data.end()) {
      run(getAdvertisingPass(&T::ID), *ctx.obj);
      auto it2 = data.find(&T::ID);
      assert(it2 != data.end() &&
             "Advertised pass did not publish promised data.");
      dataPtr = it2->second;
    } else {
      dataPtr = it->second;
    }
    return *static_cast<T *>(dataPtr);
  }

  template <typename T> void retract() {
    data.erase(&T::ID);
    publisher.erase(&T::ID);
  }

  template <typename T> void preserve() {}

  void invalidateAll() {}

private:
  void run(IRPass<PassT> &pass, PassT &obj) {
    PassContext oldCtx = ctx;
    ctx = PassContext(&pass, &obj);
    pass.run(obj, *this);
    invalidate();
    ctx = oldCtx;
  }

  void invalidate() {}

  IRPass<PassT> &getAdvertisingPass(IRInfoID ty) {
    auto it = advertiser.find(ty);
    assert(it != advertiser.end() &&
           "No pass is advertising this information.");
    return *it->second;
  }

  void clearData() {
    ctx = PassContext<PassT>();
    data.clear();
  }

  std::unordered_map<IRInfoID, void *> data;
  std::unordered_map<IRInfoID, IRPass<PassT> *> publisher;
  std::unordered_map<IRInfoID, IRPass<PassT> *> advertiser;
  PassContext<PassT> ctx;
};

template <typename PassT> class IRPipeline {
public:
  void addPass(std::unique_ptr<IRPass<PassT>> pass) {
    sequencedPasses.push_back(std::move(pass));
  }

  void addLazyPass(std::unique_ptr<IRPass<PassT>> pass) {
    info.ctx = PassContext<PassT>(pass.get(), nullptr);
    pass->advertise(info);
    lazyPasses.push_back(std::move(pass));
  }

  void run(PassT &passObj) {
    info.clearData();
    for (auto &pass : sequencedPasses) {
      info.run(*pass, passObj);
    }
  }

private:
  IRInfo<PassT> info;
  std::vector<std::unique_ptr<IRPass<PassT>>> sequencedPasses;
  std::vector<std::unique_ptr<IRPass<PassT>>> lazyPasses;
};
