#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <utility>

template <typename PassT> class IRPass;
template <typename PassT> class IRPipeline;
template <typename PassT> class IRInfo;

using IRInfoID = const void *;

template <typename PassT> class IRPass {
public:
  virtual ~IRPass() {}

  // Must be callable for different objs without calling invalidate
  virtual void run(PassT &obj, IRInfo<PassT> &info) = 0;

  virtual const char *name() = 0;
  virtual void advertise(IRInfo<PassT> &) {}
  virtual void invalidate(IRInfo<PassT> &) {}
};

template <typename PassT> class PassContext {
public:
  PassContext() {}
  PassContext(IRPass<PassT> *pass, PassT *passObj) : pass(pass), obj(passObj) {}
  IRPass<PassT> *pass = nullptr;
  PassT *obj = nullptr;
  bool preserveAll = false;
  std::unordered_set<IRInfoID> preserved;
  std::vector<IRInfoID> retracted;
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
    publisher[&T::ID] = {ctx.pass, &val};
    preserve<T>();
  }

  template <typename T> T &query() {
    auto it = publisher.find(&T::ID);
    void *dataPtr = nullptr;
    if (it == publisher.end()) {
      auto &pass = getAdvertisingPass(&T::ID);
      std::cout << "!!Running pass in query: " << pass.name() << "!!\n";
      run(pass, *ctx.obj);
      auto it2 = publisher.find(&T::ID);
      assert(it2 != publisher.end() &&
             "Advertised pass did not publish promised data.");
      dataPtr = it2->second.dataPtr;
    } else {
      dataPtr = it->second.dataPtr;
    }
    return *static_cast<T *>(dataPtr);
  }

  template <typename T> void retract() { ctx.retracted.push_back(&T::ID); }

  template <typename T> void preserve() { ctx.preserved.insert(&T::ID); }

  void preserveAll() { ctx.preserveAll = true; }

  Arch &getArch() {
    assert(arch);
    return *arch;
  }

  Arch *getArchUnchecked() { return arch; }

private:
  struct PublishedData {
    IRPass<PassT> *pass = nullptr;
    void *dataPtr = nullptr;
  };

  void run(IRPass<PassT> &pass, PassT &obj) {
    PassContext oldCtx = std::move(ctx);
    ctx = PassContext(&pass, &obj);
    pass.run(obj, *this);
    invalidate();
    ctx = std::move(oldCtx);
  }

  void invalidate() {
    assert(ctx.retracted.empty());
    if (ctx.preserveAll) {
      return;
    }
    std::unordered_set<IRPass<PassT> *> passesToInvalidate;
    for (auto &[id, data] : publisher) {
      if (ctx.preserved.contains(id))
        continue;
      passesToInvalidate.insert(data.pass);
    }
    for (auto *pass : passesToInvalidate) {
      pass->invalidate(*this);
    }
    for (auto id : ctx.retracted) {
      publisher.erase(id);
    }
  }

  IRPass<PassT> &getAdvertisingPass(IRInfoID id) {
    auto it = advertiser.find(id);
    assert(it != advertiser.end() &&
           "No pass is advertising this information.");
    return *it->second;
  }

  void reset() {
    ctx = PassContext<PassT>();
    publisher.clear();
  }

  std::unordered_map<IRInfoID, PublishedData> publisher;
  std::unordered_map<IRInfoID, IRPass<PassT> *> advertiser;
  PassContext<PassT> ctx;
  Arch *arch = nullptr;
};

template <typename PassT> class IRPipeline {
public:
  IRPipeline(Arch *arch = nullptr) { info.arch = arch; }

  void addPass(std::unique_ptr<IRPass<PassT>> pass) {
    sequencedPasses.push_back(std::move(pass));
  }

  void addLazyPass(std::unique_ptr<IRPass<PassT>> pass) {
    info.ctx = PassContext<PassT>(pass.get(), nullptr);
    pass->advertise(info);
    lazyPasses.push_back(std::move(pass));
  }

  void run(PassT &passObj) {
    info.reset();
    for (auto &pass : sequencedPasses) {
      std::cout << "!!Running pass: " << pass->name() << "!!\n";
      info.run(*pass, passObj);
    }
  }

private:
  IRInfo<PassT> info;
  std::vector<std::unique_ptr<IRPass<PassT>>> sequencedPasses;
  std::vector<std::unique_ptr<IRPass<PassT>>> lazyPasses;
};

class FunctionPipelinePass : public IRPass<Program> {
public:
  FunctionPipelinePass(IRPipeline<Function> &pipeline) : pipeline(pipeline) {}
  const char *name() override { return "FunctionPipelinePass"; }
  void run(Program &prog, IRInfo<Program> &info) override {
    for (auto &func : prog.functions) {
      pipeline.run(*func);
    }
  }

private:
  IRPipeline<Function> &pipeline;
};
