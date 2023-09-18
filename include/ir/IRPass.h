#pragma once

#include "ir/IR.h"
#include <cassert>
#include <unordered_map>

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

template <typename PassT> class IRInfo {
  friend class IRPipeline<PassT>;

public:
  IRInfo() {}

  template <typename T> void advertise() {
    assert(infoPasses.find(&T::ID) == infoPasses.end() &&
           "Multiple passes advertise the same data.");
    infoPasses.emplace(&T::ID, mPass);
  }

  template <typename T> void publish(T &val) {
    /*
    assert(infoPasses.find(&T::ID) != infoPasses.end() &&
           "Published unadvertised data");
    assert(infoPasses.find(&T::ID)->second == mPass &&
           "Published data advertised by other pass");
    */
    infoData[&T::ID] = &val;
  }

  template <typename T> void retract() {
    auto it = infoData.find(&T::ID);
    if (it == infoData.end()) {
      return;
    }
    infoData.erase(it);
  }

  template <typename T> T &query() {
    auto it = infoData.find(&T::ID);
    void *dataPtr = nullptr;
    if (it == infoData.end()) {
      run(getInfoPass(&T::ID), *mPassObj);
      auto it = infoData.find(&T::ID);
      assert(it != infoData.end() &&
             "Registered info passs did not publish promised data.");
      dataPtr = it->second;
    } else {
      dataPtr = it->second;
    }
    return *static_cast<T *>(dataPtr);
  }

  template <typename T> void preserve() {}

  void invalidate() {
    for (auto [tyId, _] : infoData) {
      mPass = &getInfoPass(tyId);
      mPass->invalidate(*this);
    }
  }

private:
  void run(IRPass<PassT> &pass, PassT &obj) {
    IRPass<PassT> *passOld = mPass;
    PassT *passObjOld = mPassObj;

    mPass = &pass;
    mPassObj = &obj;
    pass.run(obj, *this);

    mPass = passOld;
    mPassObj = passObjOld;
  }

  IRPass<PassT> &getInfoPass(IRInfoID ty) {
    auto it = infoPasses.find(ty);
    assert(it != infoPasses.end() &&
           "No pass is advertising this information.");
    return *it->second;
  }

  void clearData() {
    mPass = nullptr;
    mPassObj = nullptr;
    infoData.clear();
  }

  IRPass<PassT> *mPass = nullptr;
  PassT *mPassObj = nullptr;
  std::unordered_map<IRInfoID, void *> infoData;
  std::unordered_map<IRInfoID, IRPass<PassT> *> infoPasses;
};

template <typename PassT> class IRPipeline {
public:
  void addPass(std::unique_ptr<IRPass<PassT>> pass) {
    sequencedPasses.push_back(std::move(pass));
  }

  void addLazyPass(std::unique_ptr<IRPass<PassT>> pass) {
    info.mPass = pass.get();
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
