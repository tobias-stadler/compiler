#pragma once
#include <cassert>
#include <memory>
#include <utility>

class IntrusiveCountedPtrBase {
public:
  std::size_t getRefCount() { return refCount; }

  void incRefCount() { ++refCount; }

  void decRefCount() {
    assert(refCount > 0 && "Reference count underflow");
    --refCount;
  }

private:
  std::size_t refCount = 0;
};

template <typename T> class CountedPtr {
  template <class U> friend class CountedPtr;

public:
  CountedPtr() = default;
  CountedPtr(T *ptr) : mPtr(ptr) {
    if (mPtr) {
      mPtr->incRefCount();
    }
  }

  ~CountedPtr() { reset(); }

  CountedPtr(CountedPtr &&o) noexcept : mPtr(std::exchange(o.mPtr, nullptr)) {}

  CountedPtr &operator=(CountedPtr &&o) noexcept {
    swap(*this, o);
    return *this;
  }

  template <typename U>
  CountedPtr(CountedPtr<U> &&o) noexcept
      : mPtr(std::exchange(o.mPtr, nullptr)) {}

  template <typename U> CountedPtr &operator=(CountedPtr<U> &&o) noexcept {
    reset();
    mPtr = std::exchange(o.mPtr, nullptr);
    return *this;
  }

  CountedPtr(const CountedPtr &o) : CountedPtr(o.mPtr) {}

  CountedPtr &operator=(const CountedPtr &o) {
    CountedPtr tmp(o);
    swap(*this, tmp);
    return *this;
  }

  friend void swap(CountedPtr &lhs, CountedPtr &rhs) {
    using std::swap;
    swap(lhs.mPtr, rhs.mPtr);
  }

  void reset() noexcept {
    if (mPtr) {
      if (mPtr->getRefCount() == 1) {
        delete mPtr;
      } else {
        mPtr->decRefCount();
      }
      mPtr = nullptr;
    }
  }

  void reset(T *ptr) {
    reset();
    mPtr = ptr;
    if (mPtr) {
      mPtr->incRefCount();
    }
  }

  T *get() { return mPtr; }

  T &operator*() { return *get(); }
  T *operator->() { return get(); }

  explicit operator bool() { return mPtr; }

private:
  T *mPtr = nullptr;
};

template <typename T, typename... ARGS>
CountedPtr<T> make_counted(ARGS &&...args) {
  return CountedPtr(new T(std::forward<ARGS>(args)...));
}
