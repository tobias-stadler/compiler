#pragma once

#include <cassert>
#include <cstddef>
#include <iterator>
template <typename N, typename P> class IntrusiveList;

template <typename N, typename P> class IntrusiveListNode {
  friend class IntrusiveList<N, P>;

public:
  IntrusiveListNode() = default;
  IntrusiveListNode(IntrusiveList<N, P> *parent, IntrusiveListNode *prev,
                    IntrusiveListNode *next)
      : parent(parent), prev(prev), next(next) {}

  N &getNext() {
    assert(next);
    assert(next->isLinked());
    return *static_cast<N *>(next);
  }

  N &getPrev() {
    assert(prev);
    assert(prev->isLinked());
    return *static_cast<N *>(prev);
  }

  IntrusiveListNode &getNextNode() {
    assert(next);
    return *next;
  }

  IntrusiveListNode &getPrevNode() {
    assert(prev);
    return *prev;
  }

  P &getParent() {
    assert(isLinked());
    return *static_cast<P *>(parent);
  }

  bool isLinked() { return parent && next && prev; }

  void unlink() {
    if (!isLinked()) {
      return;
    }
    prev->next = next;
    next->prev = prev;
    parent = nullptr;
    next = nullptr;
    prev = nullptr;
  }

  void deleteThis() { delete static_cast<N *>(this); }

  void insertNext(N *o) {
    assert(o && !o->isLinked() && next && parent);
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
  IntrusiveListNode(const IntrusiveListNode &o) = delete;
  IntrusiveListNode &operator=(const IntrusiveListNode &o) = delete;
  ~IntrusiveListNode() { unlink(); }

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

  void deleteAll() {
    for (auto *node = sentryBegin.next; node != &sentryEnd;) {
      auto *tmp = node;
      node = node->next;
      tmp->parent = nullptr;
      tmp->deleteThis();
    }
  }

  N &getFirst() { return getSentryBegin().getNext(); }
  N &getLast() { return getSentryEnd().getPrev(); }

  IntrusiveListNode<N, P> &getSentryBegin() { return sentryBegin; }
  IntrusiveListNode<N, P> &getSentryEnd() { return sentryEnd; }

  class iterator {
  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = N;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;

    iterator() {}
    iterator(IntrusiveListNode<N, P> &ref) : mPtr(&ref) {}

    reference operator*() const { return static_cast<reference>(*mPtr); }
    pointer operator->() const { return static_cast<pointer>(mPtr); }

    iterator &operator++() {
      mPtr = &mPtr->getNextNode();
      return *this;
    }

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    iterator &operator--() {
      mPtr = &mPtr->getPrevNode();
      return *this;
    }

    iterator operator--(int) {
      iterator tmp(*this);
      --(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.mPtr == b.mPtr;
    }

    friend bool operator!=(const iterator &a, const iterator &b) {
      return a.mPtr != b.mPtr;
    }

  private:
    IntrusiveListNode<N, P> *mPtr = nullptr;
  };

  iterator begin() { return iterator(*sentryBegin.next); }
  iterator end() { return iterator(sentryEnd); }

protected:
  IntrusiveList(const IntrusiveList &o) = delete;
  IntrusiveList &operator=(const IntrusiveList &o) = delete;
  ~IntrusiveList() { deleteAll(); }

private:
  IntrusiveListNode<N, P> sentryBegin;
  IntrusiveListNode<N, P> sentryEnd;
};
