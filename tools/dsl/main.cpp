#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <memory>
#include <regex>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

void error(std::string_view err) {
  std::cerr << "[Error] " << err << std::endl;
  exit(EXIT_FAILURE);
}

static std::string includeDir;

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

class Token {
public:
  enum Kind {
    EMPTY,
    CUSTOM,
    CODE,
    STR,
    IDENT,
    NUM,
    PARENO,
    PARENC,
    CURLYO,
    CURLYC,
    SQUAREO,
    SQUAREC,
    EQ,
    COMMA,
    COLON,
    SLASH_FWD,
    DOT,
    SEMICOLON,
    PERCENT,
    HASH,
  };
  Kind kind = EMPTY;
  std::string_view str;

  Token() {}

  Token(Kind kind, std::string_view str) : kind(kind), str(str) {}

  operator std::string_view() { return str; }

  explicit operator bool() { return kind != EMPTY; }
};

class RecordIdent {
public:
  RecordIdent() {}
  RecordIdent(std::vector<std::string_view> idents)
      : idents(std::move(idents)) {}

  std::vector<std::string_view> idents;

  std::string_view getName() { return idents.back(); }
};

class Lexer {
public:
  std::string_view str;
  std::string_view::iterator pos;

  Token tok;

  Lexer(std::string_view str) : str(str), pos(str.begin()) { fetchToken(); }

  void fetchToken() {
    // assert(tok.kind != Token::EMPTY);
    if (!eatWhitespace()) {
      tok.kind = Token::EMPTY;
      return;
    }

    auto kind = getSingleCharKind();
    if (kind != Token::EMPTY) {
      tok.kind = kind;
      auto tmp = pos++;
      tok.str = std::string_view(tmp, pos);
      return;
    }

    static const std::regex identRegEx("([a-zA-Z_0-9]+)");
    static const std::regex numRegEx("([0-9]+)");
    static const std::regex strRegEx("\"(.*)\"");
    static const std::regex codeRegEx("[$]([^$]+)[$]");

    std::cmatch m;
    if (std::regex_search(pos, str.end(), m, identRegEx,
                          std::regex_constants::match_continuous)) {
      kind = Token::IDENT;
    } else if (std::regex_search(pos, str.end(), m, numRegEx,
                                 std::regex_constants::match_continuous)) {
      kind = Token::NUM;
    } else if (std::regex_search(pos, str.end(), m, strRegEx,
                                 std::regex_constants::match_continuous)) {
      kind = Token::STR;
    } else if (std::regex_search(pos, str.end(), m, codeRegEx,
                                 std::regex_constants::match_continuous)) {
      kind = Token::CODE;
    }
    if (kind != Token::EMPTY) {
      tok.str = std::string_view(m[1].first, m[1].second);
      tok.kind = kind;
      pos = m[0].second;
      return;
    }
    error(std::format("Invalid token: {}", *pos));
  }

  Token::Kind getSingleCharKind() {
    switch (*pos) {
    case '{':
      return Token::CURLYO;
    case '}':
      return Token::CURLYC;
    case '(':
      return Token::PARENO;
    case ')':
      return Token::PARENC;
    case '[':
      return Token::SQUAREO;
    case ']':
      return Token::SQUAREC;
    case '=':
      return Token::EQ;
    case ':':
      return Token::COLON;
    case ',':
      return Token::COMMA;
    case ';':
      return Token::SEMICOLON;
    case '%':
      return Token::PERCENT;
    case '#':
      return Token::HASH;
    case '/':
      return Token::SLASH_FWD;
    case '.':
      return Token::DOT;
    default:
      return Token::EMPTY;
    }
  }

  Token peekToken() { return tok; }

  Token::Kind peekTokenKind() { return tok.kind; }

  Token nextToken() {
    Token tmp = tok;
    fetchToken();
    return tmp;
  }

  bool eatWhitespace() {
    while (true) {
      if (pos == str.end()) {
        return false;
      }
      if (isspace(*pos)) {
        ++pos;
      } else {
        return true;
      }
    }
  }

  bool matchPeekToken(Token::Kind kind) {
    if (tok.kind != kind)
      return false;

    return true;
  }

  bool matchPeekTokenOrEmpty(Token::Kind kind) {
    if (tok.kind == kind || tok.kind == Token::EMPTY) {
      return true;
    }
    return false;
  }

  bool matchNextToken(Token::Kind kind) {
    if (tok.kind != kind)
      return false;

    fetchToken();
    return true;
  }

  bool matchNextTokenOrEmpty(Token::Kind kind) {
    if (tok.kind != kind && tok.kind != Token::EMPTY)
      return false;

    fetchToken();
    return true;
  }

  Token expectNextToken(Token::Kind kind) {
    if (tok.kind != kind)
      error(std::string("Expected different token. Got: ")
                .append(std::string_view(tok)));

    return nextToken();
  }

  void expectNextIdent(std::string_view ident) {
    if (expectNextToken(Token::IDENT) != ident) {
      error("Expected different ident");
    }
  }

  bool matchNextIdent(std::string_view ident) {
    if (tok.kind == Token::IDENT && tok == ident) {
      fetchToken();
      return true;
    }
    return false;
  }

  Token expectSSAName() {
    matchNextToken(Token::PERCENT);
    return expectNextToken(Token::IDENT);
  }

  Token expectPlaceholderName() {
    matchNextToken(Token::HASH);
    return expectNextToken(Token::IDENT);
  }

  void expectDoubleColon() {
    expectNextToken(Token::COLON);
    expectNextToken(Token::COLON);
  }

  RecordIdent expectRecordIdent() {
    std::vector<std::string_view> res;
    res.push_back(expectNextToken(Token::IDENT));
    while (matchPeekToken(Token::COLON)) {
      expectDoubleColon();
      res.push_back(expectNextToken(Token::IDENT));
    }
    return res;
  }
};

class RecordSpace;

class Record {
public:
  virtual ~Record() {}

  virtual void parse(Lexer &lex) {}

  std::string_view name;
  std::string_view type;
  RecordSpace *parent = nullptr;
};

class RecordFactory {
public:
  using factory_func = std::function<std::unique_ptr<Record>()>;

  std::unique_ptr<Record> createRecord(std::string_view type) {
    auto it = recTemplates.find(type);
    if (it == recTemplates.end()) {
      return nullptr;
    }
    auto rec = it->second();
    rec->type = type;
    return rec;
  }

  void registerRecord(const char *type, factory_func func) {
    recTemplates.try_emplace(std::string_view(type), func);
  }

private:
  std::unordered_map<std::string_view, factory_func> recTemplates;
};

class Parser {
public:
  Parser(Lexer &lex, RecordFactory &factory) : lex(lex), factory(factory) {}

  std::vector<std::unique_ptr<Record>> parseRecords() {
    std::vector<std::unique_ptr<Record>> recs;
    while (!lex.matchPeekTokenOrEmpty(Token::CURLYC)) {
      recs.push_back(parseRecord());
    }
    return recs;
  }

  std::unique_ptr<Record> parseRecord() {
    std::string_view recName;
    if (lex.matchNextIdent("let")) {
      recName = lex.expectNextToken(Token::IDENT);
      lex.expectNextToken(Token::EQ);
    }
    std::string_view recType = lex.expectNextToken(Token::IDENT);
    auto recPtr = factory.createRecord(recType);
    if (!recPtr) {
      error(std::format("Invalid record type: {}", recType));
    }
    recPtr->name = recName;
    lex.expectNextToken(Token::CURLYO);
    recPtr->parse(lex);
    lex.expectNextToken(Token::CURLYC);
    return recPtr;
  }

  Lexer &lex;
  RecordFactory &factory;
};

class RecordSpace : public Record {
public:
  RecordSpace(RecordFactory &fac) : fac(fac) {}
  void parse(Lexer &lex) override {
    records = Parser(lex, fac).parseRecords();
    for (size_t i = 0; i < records.size(); ++i) {
      auto &rec = *records[i];
      rec.parent = this;
      if (rec.name.empty() || rec.name == "_") {
        continue;
      }
      auto [_, succ] = recordIndex.try_emplace(rec.name, i);
      if (!succ) {
        error("Redeclared record");
      }
    }
  }

  Record *getRecord(std::string_view recName) {
    auto it = recordIndex.find(recName);
    if (it == recordIndex.end()) {
      return nullptr;
    }
    return records[it->second].get();
  }

  RecordFactory &fac;
  std::vector<std::unique_ptr<Record>> records;
  std::unordered_map<std::string_view, size_t> recordIndex;
};

class SymbolTable {
public:
  Record *getRecord(std::string_view name) {
    for (auto &rs : scopes) {
      auto *rec = rs->getRecord(name);
      if (rec) {
        return rec;
      }
    }
    return nullptr;
  }

  Record *lookupPath(const RecordIdent &recIdent) {
    if (recIdent.idents.size() < 1) {
      return nullptr;
    }
    Record *rec = getRecord(recIdent.idents[0]);
    for (int i = 1; i < recIdent.idents.size(); ++i) {
      RecordSpace *rs = dynamic_cast<RecordSpace *>(rec);
      if (!rs) {
        return nullptr;
      }
      rec = rs->getRecord(recIdent.idents[i]);
    }
    return rec;
  }

  void pushScope(RecordSpace &rs) { scopes.push_back(&rs); }

  void popScope() { scopes.pop_back(); }

  std::vector<RecordSpace *> scopes;
};

class DSLListRecord : public Record {
public:
  void parse(Lexer &lex) override {
    while (lex.matchPeekToken(Token::IDENT)) {
      refs.emplace_back(lex.expectRecordIdent());
    }
  }

  std::vector<Record *> genRecs(SymbolTable &sym) {
    std::vector<Record *> res;
    for (auto &ref : refs) {
      auto *rec = sym.lookupPath(ref);
      if (!rec) {
        error("dsl_list: record not found");
      }
      res.push_back(rec);
    }
    return res;
  }

  std::vector<RecordIdent> refs;
};

class UsingRecord : public DSLListRecord {
public:
  void pushSymbols(SymbolTable &sym) {
    auto refs = genRecs(sym);
    for (auto *ref : refs) {
      RecordSpace *rs = dynamic_cast<RecordSpace *>(ref);
      if (!rs) {
        error("Reference in using record isn't RecordSpace");
      }
      sym.pushScope(*rs);
    }
  }
  void popSymbols(SymbolTable &sym) {
    for (int i = 0; i < refs.size(); ++i) {
      sym.popScope();
    }
  }
};

class CodeBuilder {
public:
  CodeBuilder &startFunction(std::string_view signature) {
    header << signature << ";\n";
    indent().print(signature).print(" {\n");
    incIndent();
    return *this;
  }

  CodeBuilder &startBlock(std::string_view hdr) {
    if (hdr.empty()) {
      println("{");
    } else {
      indent().print(hdr).print(" {\n");
    }
    incIndent();
    return *this;
  }

  CodeBuilder &endBlock() {
    decIndent();
    println("}");
    return *this;
  }

  CodeBuilder &endBlockSemicolon() {
    decIndent();
    println("};");
    return *this;
  }

  CodeBuilder &incIndent() {
    ++indentationLevel;
    return *this;
  }

  CodeBuilder &decIndent() {
    --indentationLevel;
    return *this;
  }

  CodeBuilder &print(std::string_view txt) {
    body << txt;
    return *this;
  }

  CodeBuilder &println(std::string_view txt = std::string_view()) {
    if (!txt.empty()) {
      indent();
    }
    body << txt << "\n";
    return *this;
  }

  CodeBuilder &indent() {
    for (int i = 0; i < indentationLevel; ++i) {
      body << "  ";
    }
    return *this;
  }

  std::ostringstream header;
  std::ostringstream body;
  int indentationLevel = 0;
};

class IncludeRecord : public RecordSpace {
public:
  IncludeRecord(RecordFactory &fac) : RecordSpace(fac) {}

  void parse(Lexer &lex) override {
    childContent = loadFile(
        includeDir + '/' +
        std::string(std::string_view(lex.expectNextToken(Token::STR))));
    Lexer childLex(childContent);
    RecordSpace::parse(childLex);
  }

  std::string childContent;
};

class IRPatternRecord : public Record {

public:
  IRPatternRecord() {}

  struct OperandPat {
    enum Kind {
      SSA_DEF,
      SSA_USE,
      PLACEHOLDER,
    };
    Kind kind;
    std::string_view name;
    std::string_view type;
    std::string_view code;
    bool isGenerated = false;
  };

  struct InstrPat {
    RecordIdent opcode;
    std::vector<OperandPat> operands;
    bool isGenerated = false;

    static InstrPat parse(Lexer &lex) {
      InstrPat instr;
      instr.opcode = lex.expectRecordIdent();
      while (!lex.matchNextTokenOrEmpty(Token::SEMICOLON)) {
        instr.operands.emplace_back();
        OperandPat &op = instr.operands.back();
        if (lex.matchPeekToken(Token::PERCENT)) {
          op.kind = OperandPat::SSA_USE;
          op.name = lex.expectSSAName();
          continue;
        }
        if (lex.matchPeekToken(Token::IDENT)) {
          lex.expectNextIdent("def");
          op.kind = OperandPat::SSA_DEF;
          lex.expectNextToken(Token::PARENO);
          op.name = lex.expectSSAName();
          lex.expectNextToken(Token::COMMA);
          op.type = lex.expectNextToken(Token::IDENT);
          lex.expectNextToken(Token::PARENC);
          continue;
        }
        if (lex.matchPeekToken(Token::HASH)) {
          op.kind = OperandPat::PLACEHOLDER;
          op.name = lex.expectPlaceholderName();
          continue;
        }
        error("Invalid operand pattern");
      }
      return instr;
    }

    std::string genInstrKind(SymbolTable &sym) {
      Record *rec = sym.lookupPath(opcode);
      if (!rec || rec->type != "Instr" || rec->parent->type != "Arch") {
        return std::format("Instr::{}", opcode.getName());
      }
      return std::format("{}::{}", rec->parent->name, opcode.getName());
    }
  };

  struct InstrPats {
    std::vector<InstrPat> instrs;
    void parse(Lexer &lex) {
      while (!lex.matchPeekToken(Token::CURLYC)) {
        instrs.push_back(InstrPat::parse(lex));
      }
    }
  };

  struct IfPat {
    void parse(Lexer &lex) {
      while (true) {
        switch (lex.peekTokenKind()) {
        case Token::CURLYC:
          return;
        default:
          error("[ir_pat] Invalid token in if");
          break;
        case Token::CODE:
          code.push_back(lex.nextToken());
          break;
        case Token::HASH:
        case Token::PERCENT:
          lex.fetchToken();
          code.push_back(lex.expectNextToken(Token::IDENT));
          break;
        }
      }
    }
    std::vector<Token> code;
  };

  std::string genType(std::string_view tyStr) {
    static std::regex intRE("i([0-9]+)");
    std::cmatch m;
    if (std::regex_match(tyStr.begin(), tyStr.end(), m, intRE)) {
      return std::format("IntSSAType::get({})", m[1].str());
    }
    error("Invalid ir type");
    return std::string();
  }

  void genInstrMatch(InstrPat &instrPat, std::string_view instrVar,
                     CodeBuilder &code, SymbolTable &sym) {
    if (instrPat.isGenerated) {
      return;
    }

    code.println(std::format("if({}.getKind() != {}) return false;", instrVar,
                             instrPat.genInstrKind(sym)));
    instrPat.isGenerated = true;
    unsigned opNum = 0;
    for (auto &op : instrPat.operands) {
      switch (op.kind) {
      case OperandPat::SSA_DEF:
        if (op.name != "_") {
          code.println(std::format("auto &m_def_{} = {}.getOperand({});",
                                   op.name, instrVar, opNum));
        }
        if (op.type != "_") {
          code.println(std::format(
              "if({}.getOperand({}).ssaDefType() != {}) return false;",
              instrVar, opNum, genType(op.type)));
        }
        break;
      case OperandPat::SSA_USE: {
        if (op.name == "_")
          break;
        auto it = matchSSADefs.find(op.name);
        assert(it != matchSSADefs.end());
        auto [usePat, useOpNum] = it->second;
        if (usePat->operands[useOpNum].kind == OperandPat::SSA_USE) {
          if (usePat == &instrPat) {
            code.println(std::format(
                "auto &m_def_{} = {}.getOperand({}).ssaUse().getDef();",
                op.name, instrVar, opNum));
          } else if (usePat->isGenerated) {
            code.println(
                std::format("if(&{}.getOperand({}).ssaUse().getDef() != "
                            "&m_def_{}) return false;",
                            instrVar, opNum, op.name));
          } else {
            error("Cannot match SSA use against ssa use in unconstrained "
                  "instruction");
          }
        } else {
          code.println(std::format("auto &m_def_{}_instr = "
                                   "{}.getOperand({}).ssaUse().getDefInstr();",
                                   op.name, instrVar, opNum));
          genInstrMatch(*usePat, std::format("m_def_{}_instr", op.name), code,
                        sym);
        }
        break;
      }
      case OperandPat::PLACEHOLDER:
        if (op.name == "_")
          break;
        code.println(std::format("auto &m_ph_{} = {}.getOperand({});", op.name,
                                 instrVar, opNum));
        break;
      }
      op.isGenerated = true;
      ++opNum;
    }
  }

  void genMatch(InstrPats &pats, CodeBuilder &code, SymbolTable &sym) {
    collectMatchSSADefs(pats);
    genInstrMatch(pats.instrs.back(), "m_root", code, sym);
    for (auto &instr : pats.instrs) {
      if (!instr.isGenerated) {
        error("Ungenerated instruction pattern");
      }
      instr.isGenerated = false;
    }
  }

  void genIf(IfPat &pat, CodeBuilder &code) {
    code.indent();
    code.print("if(!(");
    for (auto &tok : pat.code) {
      if (tok.kind == Token::CODE) {
        code.print(tok);
      } else if (tok.kind == Token::HASH) {
      } else if (tok.kind == Token::PERCENT) {
      } else {
        code.print(std::format("m_ph_{}", tok.str));
      }
    }
    code.print(")) return false;\n");
  }

  void genEmit(InstrPats &pats, CodeBuilder &code, SymbolTable &sym) {
    unsigned instrNum = 0;
    for (auto &instr : pats.instrs) {
      code.println(std::format("Instr *e_instr_{} = new Instr({});", instrNum,
                               instr.genInstrKind(sym)));
      code.println(std::format("m_root.insertPrev(e_instr_{});", instrNum));
      code.println(std::format("e_instr_{}->allocateOperands({});", instrNum,
                               instr.operands.size()));
      for (auto &op : instr.operands) {
        if (op.kind == OperandPat::SSA_DEF) {
          code.println(std::format(
              "e_instr_{}->emplaceOperand<Operand::SSA_DEF_TYPE>({});",
              instrNum, genType(op.type)));
          if (op.name != "_") {
            code.println(
                std::format("auto &e_def_{} = e_instr_{}->getLastOperand();",
                            op.name, instrNum));
            auto it = matchSSADefs.find(op.name);
            if (it != matchSSADefs.end()) {
              code.println(
                  std::format("m_def_{}.ssaDef().replaceAllUses(e_def_{});",
                              op.name, op.name));
              matchSSADefs.erase(it);
            }
          }
        } else if (op.kind == OperandPat::SSA_USE) {
          auto it = matchSSADefs.find(op.name);
          code.println(std::format(
              "e_instr_{}->emplaceOperand<Operand::SSA_USE>({}_def_{});",
              instrNum, it == matchSSADefs.end() ? "e" : "m", op.name));
        } else if (op.kind == OperandPat::PLACEHOLDER) {
          code.println(std::format(
              "e_instr_{}->emplaceOperand<Operand::EMPTY>();", instrNum));
          code.println(std::format("e_instr_{}->getLastOperand() = m_ph_{};",
                                   instrNum, op.name));
        } else {
          error("Unsupported operand pattern in emit");
        }
      }
      ++instrNum;
    }
  }

  void collectMatchSSADefs(InstrPats &m) {
    for (auto &instr : m.instrs) {
      unsigned opNum = 0;
      for (auto &op : instr.operands) {
        if ((op.kind == OperandPat::SSA_DEF ||
             op.kind == OperandPat::SSA_USE) &&
            op.name != "_") {
          auto [_, succ] = matchSSADefs.try_emplace(op.name, &instr, opNum);
          if (op.kind == OperandPat::SSA_DEF && !succ) {
            error("SSA Def redeclared");
          }
        }
        ++opNum;
      }
    }
  }

  void parse(Lexer &lex) override {
    while (!lex.matchPeekTokenOrEmpty(Token::CURLYC)) {
      std::string_view recType = lex.expectNextToken(Token::IDENT);
      lex.expectNextToken(Token::CURLYO);
      if (recType == "match") {
        if (matchPats.instrs.size() != 0) {
          error("Multiple match patterns in ir_pat");
        }
        matchPats.parse(lex);
      } else if (recType == "emit") {
        if (emitPats.instrs.size() != 0) {
          error("Multiple emit patterns in ir_pat");
        }
        emitPats.parse(lex);
      } else if (recType == "if") {
        ifPats.emplace_back();
        ifPats.back().parse(lex);
      } else {
        error("Invalid operator in ir_pat");
      }
      lex.expectNextToken(Token::CURLYC);
    }
  }

  void gen(CodeBuilder &code, SymbolTable &sym) {
    genMatch(matchPats, code, sym);
    if (matchPats.instrs.size() == 0) {
      error("[ir_pat] missing match pattern");
    }
    for (auto &pat : ifPats) {
      genIf(pat, code);
    }
    if (emitPats.instrs.size() > 0) {
      code.println();
      genEmit(emitPats, code, sym);
    }
    code.println("return true;");
  }

  std::unordered_map<std::string_view, std::pair<InstrPat *, unsigned>>
      matchSSADefs;

  InstrPats matchPats;
  InstrPats emitPats;
  std::vector<IfPat> ifPats;
};

void genArch(RecordSpace &rs, CodeBuilder &code, SymbolTable &sym) {
  std::vector<RecordSpace *> regRecs;
  std::vector<RecordSpace *> regClassRecs;
  std::vector<RecordSpace *> instrRecs;
  for (auto &rec : rs.records) {
    if (rec->type == "Register") {
      regRecs.push_back(dynamic_cast<RecordSpace *>(rec.get()));
    } else if (rec->type == "Instr") {
      instrRecs.push_back(dynamic_cast<RecordSpace *>(rec.get()));
    } else if (rec->type == "RegClass") {
      regClassRecs.push_back(dynamic_cast<RecordSpace *>(rec.get()));
    }
  }
  code.startBlock("enum RegisterKind");
  for (auto rec : regRecs) {
    code.println(std::format("{},", rec->name));
  }
  code.endBlockSemicolon();
  code.startBlock("enum InstrKind");
  code.println("TARGET_INSTR_START = Instr::TARGET_INSTR,");
  for (auto rec : instrRecs) {
    code.println(std::format("{},", rec->name));
  }
  code.endBlockSemicolon();
  code.startBlock("enum RegClassKind");
  for (auto rec : regClassRecs) {
    code.println(std::format("{},", rec->name));
  }
  code.endBlockSemicolon();
}

void genInstrSelector(RecordSpace &rs, CodeBuilder &code, SymbolTable &sym) {
  std::vector<IRPatternRecord *> recs;
  std::transform(
      rs.records.begin(), rs.records.end(), std::back_inserter(recs),
      [](auto &rec) { return &dynamic_cast<IRPatternRecord &>(*rec); });
  std::stable_sort(recs.begin(), recs.end(),
                   [](IRPatternRecord *a, IRPatternRecord *b) {
                     return a->matchPats.instrs.back().opcode.getName() <
                            b->matchPats.instrs.back().opcode.getName();
                   });
  int numSameOpc = 0;
  std::string_view lastOpc;
  for (auto *rec : recs) {
    std::string_view opcName = rec->matchPats.instrs.back().opcode.getName();
    code.startFunction(
        std::format("bool dslPat{}{}(Instr &m_root)", opcName, numSameOpc));
    rec->gen(code, sym);
    code.endBlock();
    if (opcName == lastOpc) {
      ++numSameOpc;
    } else {
      numSameOpc = 0;
      lastOpc = opcName;
    }
  }
  numSameOpc = 0;
  lastOpc = std::string_view();
  code.startFunction("bool dslSelectInstr(Instr &m_root)");
  code.startBlock("switch(m_root.getKind())");
  code.println("default:");
  for (auto *rec : recs) {
    std::string_view opcName = rec->matchPats.instrs.back().opcode.getName();
    if (opcName == lastOpc) {
      ++numSameOpc;
    } else {
      code.println("break;");
      code.println(std::format("case {}: ",
                               rec->matchPats.instrs.back().genInstrKind(sym)));
      numSameOpc = 0;
      lastOpc = opcName;
    }
    code.println(std::format("if(dslPat{}{}(m_root)) return true;", opcName,
                             numSameOpc));
  }
  code.println("break;");
  code.endBlock();
  code.println("return false;");
  code.endBlock();
}

int main(int argc, char *argv[]) {
  if (argc != 5) {
    std::cerr << "Expected <in-file> <out-file> <record> <include-dir>"
              << std::endl;
    return EXIT_FAILURE;
  }
  includeDir = argv[4];

  RecordFactory fac;
  RecordFactory facIRPat;
  fac.registerRecord("ir_pat",
                     [&]() { return std::make_unique<IRPatternRecord>(); });
  fac.registerRecord("Arch",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("InstrSelector",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("dsl_list",
                     [&]() { return std::make_unique<DSLListRecord>(); });
  fac.registerRecord("Register",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("Instr",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("RegClass",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("include",
                     [&] { return std::make_unique<IncludeRecord>(fac); });
  fac.registerRecord("using", [&] { return std::make_unique<UsingRecord>(); });

  std::string str = loadFile(argv[1]);
  Lexer lex(str);
  RecordSpace rs(fac);

  rs.parse(lex);
  SymbolTable sym;
  CodeBuilder code;
  sym.pushScope(rs);

  std::string_view genRecName = argv[3];
  auto *genRec = rs.getRecord(genRecName);
  if (!genRec) {
    error("Can't find this record");
  }

  for (auto &rec : rs.records) {
    if (rec->type == "using") {
      dynamic_cast<UsingRecord &>(*rec.get()).pushSymbols(sym);
    }
  }

  if (genRec->type == "Arch") {
    genArch(dynamic_cast<RecordSpace &>(*genRec), code, sym);
  } else if (genRec->type == "InstrSelector") {
    genInstrSelector(dynamic_cast<RecordSpace &>(*genRec), code, sym);
  } else {
    error("Can't generate this record type");
  }

  std::ofstream outFile(argv[2]);
  if (!outFile.good()) {
    std::cerr << "Couldn't open out file" << std::endl;
    return EXIT_FAILURE;
  }
  outFile << code.body.str();
  return EXIT_SUCCESS;
}
