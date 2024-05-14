#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include <cassert>
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

template <typename PassT> class IRPipelineInfo;

template <typename PassT> struct IRPassContext {
  IRPassContext() {}
  IRPassContext(IRPipelineInfo<PassT> *pipeline, IRPass<PassT> *pass,
                PassT *passObj)
      : pipeline(pipeline), pass(pass), obj(passObj) {}
  IRPipelineInfo<PassT> *pipeline = nullptr;
  IRPass<PassT> *pass = nullptr;
  PassT *obj = nullptr;
  bool preserveAll = false;
  std::unordered_set<IRInfoID> preserved;
  std::vector<IRInfoID> retracted;
};

template <typename PassT> class IRInfo {
public:
  IRInfo(IRPassContext<PassT> &ctx) : ctx(ctx) {}

  template <typename T> void advertise() {
    ctx.pipeline->advertise(ctx, &T::ID);
  }

  template <typename T> void publish(T &val) {
    ctx.pipeline->publish(ctx, &T::ID, &val);
  }

  template <typename T> T &query() {
    return *static_cast<T *>(ctx.pipeline->runQuery(ctx, &T::ID));
  }

  template <typename T> void retract() { ctx.retracted.push_back(&T::ID); }

  template <typename T> void preserve() { ctx.preserved.insert(&T::ID); }

  template <typename T> void invalidate() {
    ctx.pipeline->runInvalidate(ctx, &T::ID);
  }

  void preserveAll() { ctx.preserveAll = true; }

  Arch &getArch() {
    assert(ctx.pipeline->arch);
    return *ctx.pipeline->arch;
  }

  Arch *getArchUnchecked() { return ctx.pipeline->arch; }

private:
  IRPassContext<PassT> &ctx;
};

template <typename PassT> class IRPipelineInfo {
  friend class IRInfo<PassT>;
  friend class IRPipeline<PassT>;

public:
  struct PublishedData {
    IRPass<PassT> *pass = nullptr;
    void *dataPtr = nullptr;
  };

  void reset() { publisher.clear(); }

  void advertise(IRPassContext<PassT> &ctx, IRInfoID id) {
    assert(advertiser.find(id) == advertiser.end() &&
           "Multiple passes advertise the same data.");
    advertiser.try_emplace(id, ctx.pass);
  }

  void runAdvertise(IRPass<PassT> &pass) {
    IRPassContext<PassT> ctx(this, &pass, nullptr);
    IRInfo<PassT> info(ctx);
    pass.advertise(info);
  }

  void publish(IRPassContext<PassT> &ctx, IRInfoID id, void *val) {
    publisher[id] = {ctx.pass, val};
    ctx.preserved.insert(id);
  }

  void *runQuery(IRPassContext<PassT> &ctx, IRInfoID id) {
    auto it = publisher.find(id);
    void *dataPtr = nullptr;
    if (it == publisher.end()) {
      auto &pass = getAdvertisingPass(id);
      std::cout << "----> Running pass in query: " << pass.name() << "\n";
      run(pass, *ctx.obj);
      auto it2 = publisher.find(id);
      assert(it2 != publisher.end() &&
             "Advertised pass did not publish promised data.");
      dataPtr = it2->second.dataPtr;
    } else {
      dataPtr = it->second.dataPtr;
    }
    assert(dataPtr);
    return dataPtr;
  }

  void run(IRPass<PassT> &pass, PassT &obj) {
    IRPassContext<PassT> ctx(this, &pass, &obj);
    IRInfo<PassT> info(ctx);
    pass.run(obj, info);
    runInvalidate(ctx);
  }

  void runInvalidate(IRPassContext<PassT> &ctx, IRInfoID id) {
    auto it = publisher.find(id);
    if (it == publisher.end())
      return;
    auto *pass = it->second.pass;
    IRPassContext<PassT> invalidateCtx(this, pass, ctx.obj);
    IRInfo<PassT> info(invalidateCtx);
    pass->invalidate(info);
    for (auto id : invalidateCtx.retracted) {
      publisher.erase(id);
    }
  }

  void runInvalidate(IRPassContext<PassT> &ctx) {
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
    IRInfo<PassT> info(ctx);
    for (auto *pass : passesToInvalidate) {
      pass->invalidate(info);
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

private:
  std::unordered_map<IRInfoID, PublishedData> publisher;
  std::unordered_map<IRInfoID, IRPass<PassT> *> advertiser;
  Arch *arch = nullptr;
};

template <typename PassT> class IRPipeline {
public:
  IRPipeline(Arch *arch = nullptr) { info.arch = arch; }

  void addPass(std::unique_ptr<IRPass<PassT>> pass) {
    sequencedPasses.push_back(std::move(pass));
  }

  void addLazyPass(std::unique_ptr<IRPass<PassT>> pass) {
    info.runAdvertise(*pass);
    lazyPasses.push_back(std::move(pass));
  }

  void run(PassT &passObj) {
    info.reset();
    for (auto &pass : sequencedPasses) {
      std::cout << "--> Running pass: " << pass->name() << "\n";
      info.run(*pass, passObj);
    }
  }

private:
  IRPipelineInfo<PassT> info;
  std::vector<std::unique_ptr<IRPass<PassT>>> sequencedPasses;
  std::vector<std::unique_ptr<IRPass<PassT>>> lazyPasses;
};

class FunctionPipelinePass : public IRPass<Program> {
public:
  FunctionPipelinePass(IRPipeline<Function> &pipeline) : pipeline(pipeline) {}
  const char *name() override { return "FunctionPipelinePass"; }
  void run(Program &prog, IRInfo<Program> &info) override {
    for (auto &func : prog.functions()) {
      pipeline.run(func);
    }
  }

private:
  IRPipeline<Function> &pipeline;
};
