#include "Record.h"
#include "Lexer.h"

#include <cassert>
#include <format>
#include <fstream>
#include <string_view>

RecordFactory TemplateRecord::facImpl{
    {"template_var",
     []() { return std::make_unique<TemplateRecord::VarRecord>(); }},
};

void TokenRecord::parse(Lexer &lex) {
  unsigned braces = 1;
  while (true) {
    switch (lex.peekKind()) {
    case Token::CURLYO:
      ++braces;
      break;
    case Token::CURLYC:
      --braces;
      break;
    case Token::EMPTY:
      error("Token empty in TokenRecord");
      break;
    default:
      break;
    }
    if (braces == 0) {
      break;
    } else {
      toks.emplace_back(lex.next());
    }
  }
}

void RecordSpace::parse(Lexer &lex) {
  while (!lex.matchOrEmpty(Token::CURLYC)) {
    RecordHdr hdr = lex.expectRecordHdr();
    lex.expectNext(Token::CURLYO);
    std::unique_ptr<Record> recPtr;
    if (hdr.isTemplateInstance) {
      auto instRecPtr = std::make_unique<InstanceRecord>(fac);
      instRecPtr->templateName = hdr.type;
      recPtr = std::move(instRecPtr);
    } else {
      recPtr = fac.createRecord(hdr.type);
      if (!recPtr) {
        error(std::format("Invalid record type: {}", hdr.type));
      }
    }
    recPtr->name = hdr.name;
    recPtr->parse(lex);
    lex.expectNext(Token::CURLYC);
    addRecord(std::move(recPtr));
  }
}

void InstanceRecord::materialize(TemplateRecord &tempRec) {
  for (auto &recPtr : tempRec.records) {
    auto *tokRec = dynamic_cast<TokenRecord *>(recPtr.get());
    if (!tokRec)
      continue;
    TemplatedTokenSource tokSrc(*this, *tokRec);
    Lexer lex(tokSrc);
    auto rec = fac.createRecord(tokRec->realType);
    rec->parse(lex);
    addRecord(std::move(rec));
  }
}

Token TemplatedTokenSource::fetchToken() {
  if (subTokRec) {
    if (subPos < subTokRec->toks.size()) {
      return subTokRec->toks[subPos++];
    }
    subTokRec = nullptr;
  }
  if (pos >= tokRec.toks.size()) {
    return Token();
  }
  Token &tok = tokRec.toks[pos++];
  if (tok.kind == Token::IDENT) {
    subTokRec = dynamic_cast<TokenRecord *>(instanceRec.getRecord(tok.str));
    if (subTokRec && subTokRec->toks.size() > 0) {
      subPos = 0;
      return subTokRec->toks[subPos++];
    }
    subTokRec = nullptr;
  }
  return tok;
}

void IncludeRecord::parse(Lexer &lex) {
  content = loadFile(std::string(includeDir) + '/' +
                     std::string(std::string_view(lex.expectNext(Token::STR))));
  parseContent();
}

void IncludeRecord::parseContent() {
  StringTokenSource tokSrc(content);
  Lexer childLex(tokSrc);
  RecordSpace::parse(childLex);

  gather(usingRecs, "using");
  for (auto *rec : usingRecs) {
    for (auto *usedRec : rec->genRecs()) {
      auto *usedRS = dynamic_cast<RecordSpace *>(usedRec);
      if (!usedRS) {
        error("Using record references invalid record space");
      }
      usedRecs.push_back(usedRS);
    }
  }
}

void TemplateRecord::parse(Lexer &lex) {
  while (!lex.matchOrEmpty(Token::CURLYC)) {
    RecordHdr hdr = lex.expectRecordHdr();
    lex.expectNext(Token::CURLYO);
    if (hdr.isTemplateInstance) {
      error("Template instantiation not allowed in Template");
    }
    auto recPtr = std::make_unique<TokenRecord>();
    recPtr->name = hdr.name;
    recPtr->realType = hdr.type;
    recPtr->parse(lex);
    lex.expectNext(Token::CURLYC);
    addRecord(std::move(recPtr));
  }
}

std::unique_ptr<Record> RecordFactory::createRecord(std::string_view type) {
  auto it = recTemplates.find(type);
  if (it == recTemplates.end()) {
    return nullptr;
  }
  auto rec = it->second();
  rec->type = type;
  return rec;
}

void RecordFactory::registerRecord(const char *type, factory_func func) {
  recTemplates.try_emplace(std::string_view(type), func);
}

Record *RecordSpace::getRecord(std::string_view recName) {
  auto it = recordIndex.find(recName);
  if (it == recordIndex.end()) {
    return nullptr;
  }
  return records[it->second].get();
}

Record *RecordSpace::lookupRecord(std::string_view recName) {
  for (RecordSpace *rs = this; rs; rs = rs->parent) {
    Record *rec = rs->getRecord(recName);
    if (rec) {
      return rec;
    }
    if (auto *incRec = dynamic_cast<IncludeRecord *>(rs)) {
      return incRec->getRecordFromUsedRecs(recName);
    }
  }
  return nullptr;
}

Record *RecordSpace::lookupIdent(const RecordIdent &ident) {
  if (ident.idents.size() < 1) {
    return nullptr;
  }
  Record *rec = lookupRecord(ident.idents[0]);
  for (size_t i = 1; i < ident.idents.size(); ++i) {
    RecordSpace *rs = dynamic_cast<RecordSpace *>(rec);
    if (!rs) {
      return nullptr;
    }
    rec = rs->getRecord(ident.idents[i]);
  }
  return rec;
}

void RecordSpace::addRecord(std::unique_ptr<Record> recPtr) {
  assert(recPtr);
  Record *rec = recPtr.get();
  records.emplace_back(std::move(recPtr));
  rec->parent = this;
  if (rec->name.empty() || rec->name == "_") {
    return;
  }
  auto [_, succ] = recordIndex.try_emplace(rec->name, records.size() - 1);
  if (!succ) {
    error("Redeclared record");
  }
}

IncludeRecord::IncludeRecord(RecordFactory &fac, std::string_view includeDir)
    : RecordSpace(fac), includeDir(includeDir) {
  type = "include";
}

InstanceRecord::InstanceRecord(RecordFactory &fac) : RecordSpace(fac) {
  type = "Instance";
}

void BoolRecord::parse(Lexer &lex) {
  std::string_view tok = lex.expectNext(Token::IDENT);
  if (tok == "true") {
    val = true;
  } else if (tok == "false") {
    val = false;
  } else {
    error("Invalid bool value");
  }
}

std::vector<Record *> DSLListRecord::genRecs() {
  std::vector<Record *> res;
  for (auto &ref : refs) {
    auto *rec = parent->lookupIdent(ref);
    if (!rec) {
      error("dsl_list: record not found");
    }
    res.push_back(rec);
  }
  return res;
}

void DSLListRecord::parse(Lexer &lex) {
  while (lex.match(Token::IDENT)) {
    refs.emplace_back(lex.expectRecordIdent());
  }
}

TokenRecord::TokenRecord() { type = "token"; }

RecordFactory::RecordFactory(
    std::initializer_list<std::pair<const char *, factory_func>> init) {
  for (auto &i : init) {
    registerRecord(i.first, i.second);
  }
}
const char *BoolRecord::gen() { return val ? "true" : "false"; }

void RecordSpace::materializeTemplates() {
  for (auto &recPtr : records) {
    if (auto *rec = dynamic_cast<InstanceRecord *>(recPtr.get())) {
      auto *tempRec =
          dynamic_cast<TemplateRecord *>(lookupRecord(rec->templateName));
      if (!tempRec) {
        error("Cannot materialize undefined template");
      }
      rec->materialize(*tempRec);
    } else if (auto *rec = dynamic_cast<RecordSpace *>(recPtr.get())) {
      rec->materializeTemplates();
    }
  }
}

std::string loadFile(std::string_view fileName) {
  std::ifstream inFile(std::string(fileName), std::ios::ate);
  if (!inFile.good()) {
    error("Couldn't open in file\n");
  }

  auto sz = inFile.tellg();
  inFile.seekg(0);
  std::string str;
  str.resize(sz);
  inFile.read(str.data(), sz);
  return str;
}

Record *IncludeRecord::getRecordFromUsedRecs(std::string_view recName) {
  for (auto *usedRS : usedRecs) {
    auto *rec = usedRS->getRecord(recName);
    if (rec) {
      return rec;
    }
  }
  return nullptr;
}
