#pragma once

#include "Lexer.h"

#include <functional>
#include <memory>
#include <string_view>
#include <variant>
#include <vector>

class RecordSpace;

class Record {
public:
  virtual ~Record() {}

  virtual void parse(Lexer &lex) {}

  std::string_view name;
  std::string_view type;
  RecordSpace *parent = nullptr;
};

using Tokens = std::vector<Token>;
template <typename T> class TokensOrParsed {
public:
  TokensOrParsed() : data(Tokens{}) {}
  TokensOrParsed(T &&t) : data(std::move(t)) {}
  TokensOrParsed(const T &t) : data(t) {}

  Tokens &tokens() { return get<Tokens>(data); }
  T &parsed() { return get<T>(data); }

  bool isParsed() { return std::holds_alternative<T>(data); }

private:
  std::variant<Tokens, T> data;
};

class TokenRecord : public Record {
public:
  TokenRecord();

  void parse(Lexer &lex) override;

  std::string_view toString() {
    if (toks.size() != 1) {
      error("Expected string in token record");
    }
    return toks.back().str;
  }

  Tokens toks;
  TokenSource* tokSrc = nullptr;
  std::string_view realType;
};

class RecordFactory {
public:
  using factory_func = std::function<std::unique_ptr<Record>()>;

  RecordFactory() {}

  RecordFactory(
      std::initializer_list<std::pair<const char *, factory_func>> init);

  std::unique_ptr<Record> createRecord(std::string_view type);
  void registerRecord(const char *type, factory_func func);

private:
  std::unordered_map<std::string_view, factory_func> recTemplates;
};

class RecordSpace : public Record {
public:
  RecordSpace(RecordFactory &fac) : fac(fac) {}
  void parse(Lexer &lex) override;

  Record *getRecord(std::string_view recName);
  Record *lookupRecord(std::string_view name);
  Record *lookupIdent(const RecordIdent &ident);

  void addRecord(std::unique_ptr<Record> recPtr);

  template <typename T>
  void gather(std::vector<T *> &out, std::string_view type) {
    for (auto &rec : records) {
      if (rec->type == type) {
        out.push_back(dynamic_cast<T *>(rec.get()));
      }
    }
  }

  template <typename T>
  void gatherRecursively(std::vector<T *> &out, std::string_view type) {
    gather(out, type);
    for (auto &rec : records) {
      if (auto *rs = dynamic_cast<RecordSpace *>(rec.get())) {
        rs->gatherRecursively(out, type);
      }
    }
  }

  void materializeTemplates();

  RecordFactory &fac;
  std::vector<std::unique_ptr<Record>> records;
  std::unordered_map<std::string_view, size_t> recordIndex;
};

class DSLListRecord : public Record {
public:
  void parse(Lexer &lex) override;

  std::vector<Record *> genRecs();

  std::vector<RecordIdent> refs;
};

class BoolRecord : public Record {
public:
  void parse(Lexer &lex) override;

  bool val;

  const char *gen();
};

class TemplateRecord : public RecordSpace {
public:
  static RecordFactory facImpl;
  TemplateRecord() : RecordSpace(facImpl) {}
  class VarRecord : public Record {};
  void parse(Lexer &lex) override;
};

class InstanceRecord : public RecordSpace {
public:
  InstanceRecord(RecordFactory &fac);

  void materialize(TemplateRecord &tempRec);

  std::string_view templateName;
};

class TemplatedTokenSource : public TokenSource {
public:
  InstanceRecord &instanceRec;
  TokenRecord &tokRec;
  size_t pos = 0;
  size_t subPos;
  TokenRecord *subTokRec = nullptr;

  TemplatedTokenSource(InstanceRecord &instanceRec, TokenRecord &tokRec)
      : instanceRec(instanceRec), tokRec(tokRec) {}

  Token fetchToken() override;
  void dump(Token tok) override;
};

class IncludeRecord : public RecordSpace {
public:
  IncludeRecord(RecordFactory &fac, std::string_view includeDir);

  void parse(Lexer &lex) override;
  void parseContent();

  Record *getRecordFromUsedRecs(std::string_view recName);

  std::vector<DSLListRecord *> usingRecs;
  std::vector<RecordSpace *> usedRecs;
  std::string content;
  std::string_view includeDir;
  std::unique_ptr<StringTokenSource> tokSrc;
};

std::string loadFile(std::string_view fileName);
