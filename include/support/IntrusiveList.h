#pragma once

#include <cassert>
template <typename N, typename P> class IntrusiveList;

template <typename N, typename P> class IntrusiveListNode {
  friend class IntrusiveList<N, P>;

public:
  IntrusiveListNode() = default;
  IntrusiveListNode(IntrusiveList<N, P> *parent, IntrusiveListNode *prev,
                    IntrusiveListNode *next)
      : parent(parent), prev(prev), next(next) {}

  N &getNext() {
    assert(isLinked());
    return *static_cast<N *>(next);
  }

  N &getPrev() {
    assert(isLinked());
    return *static_cast<N *>(prev);
  }

  P &getParent() {
    assert(isLinked());
    return static_cast<P *>(parent);
  }

  bool isLinked() { return parent && next && prev; }

  N *unlink() {
    if (prev)
      prev->next = next;
    if (next)
      next->prev = prev;
    next = nullptr;
    prev = nullptr;
    parent = nullptr;
    return static_cast<N *>(this);
  }

  void unlinkAndDelete() { delete unlink(); }

  void insertNext(N *o) {
    assert(o && next && parent);
    o->parent = parent;
    o->next = next;
    o->prev = this;
    next->prev = o;
    next = o;
  }

  void insertPrev(N *o) {
    assert(o && prev && parent);
    o->parent = parent;
    o->next = this;
    o->prev = prev;
    prev->next = o;
    prev = o;
  }

protected:
  ~IntrusiveListNode() {
    if (isLinked()) {
      unlink();
    }
  }

private:
  IntrusiveList<N, P> *parent = nullptr;
  IntrusiveListNode *prev = nullptr;
  IntrusiveListNode *next = nullptr;
};

template <typename N, typename P> class IntrusiveList {
public:
  IntrusiveList()
      : sentryBegin(this, nullptr, &sentryEnd),
        sentryEnd(this, &sentryBegin, nullptr) {}

  void insertBegin(N *o) {
    assert(o);
    sentryBegin.insertNext(o);
  }
  void insertEnd(N *o) {
    assert(o);
    sentryEnd.insertPrev(o);
  }

  IntrusiveListNode<N, P> &getSentryBegin() { return sentryBegin; }
  IntrusiveListNode<N, P> &getSentryEnd() { return sentryEnd; }

protected:
  ~IntrusiveList() {}

private:
  IntrusiveListNode<N, P> sentryBegin;
  IntrusiveListNode<N, P> sentryEnd;
};
