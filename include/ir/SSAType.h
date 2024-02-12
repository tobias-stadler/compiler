#pragma once

class SSAType {
public:
  enum Kind {
    VOID,
    INT,
    PTR,
  };
  static constexpr const char *kindName(Kind kind) {
    switch (kind) {
    case VOID:
      return "void";
    case INT:
      return "i";
    case PTR:
      return "ptr";
    default:
      return "?";
    }
  }

  friend bool operator==(const SSAType &a, const SSAType &b) {
    return &a == &b;
  }
  friend bool operator!=(const SSAType &a, const SSAType &b) {
    return &a != &b;
  }

  Kind getKind() const { return kind; }

protected:
  SSAType(Kind kind) : kind(kind) {}
  ~SSAType() {}

private:
  Kind kind;
};

class VoidSSAType : public SSAType {
public:
  static VoidSSAType &get() { return instance; }

private:
  VoidSSAType() : SSAType(VOID) {}
  static VoidSSAType instance;
};

class IntSSAType : public SSAType {
public:
  static bool is_impl(const SSAType &o) { return o.getKind() == INT; }

  static IntSSAType &get(unsigned bits);

  unsigned getBits() const { return bits; }

private:
  IntSSAType(unsigned bits) : SSAType(INT), bits(bits) {}
  unsigned bits;
};

class PtrSSAType : public SSAType {
public:
  static PtrSSAType &get() { return instance; }

private:
  PtrSSAType() : SSAType(PTR) {}
  static PtrSSAType instance;
};
