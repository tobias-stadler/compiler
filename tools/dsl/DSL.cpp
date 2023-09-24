#include <cctype>
#include <cstdlib>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
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

class Token {
public:
  enum Kind {
    EMPTY,
    IDENT,
    NUM,
    PARENO,
    PARENC,
    CURLYO,
    CURLYC,
    SQUAREO,
    SQUAREC,
    EQ,
    DOLLAR,
    COMMA,
    COLON,
    SEMICOLON,
    PERCENT,
    HASH,
  };
  Kind kind = EMPTY;
  std::string_view str;

  operator std::string_view() { return str; }

  explicit operator bool() { return kind != EMPTY; }
};

class Lexer {
public:
  std::string_view str;
  std::string_view::iterator pos;

  Token tok;

  Lexer(std::string_view str) : str(str), pos(str.begin()) { getToken(); }

  void getToken() {
    while (true) {
      if (pos == str.end()) {
        tok.kind = Token::EMPTY;
        return;
      }
      if (isspace(*pos)) {
        ++pos;
        continue;
      }
      break;
    }

    auto kind = eatSingleChar();
    if (kind != Token::EMPTY) {
      tok.kind = kind;
      auto tmp = pos++;
      tok.str = std::string_view(tmp, pos);
      return;
    }

    static const std::regex identRegEx("[a-zA-Z_0-9]+");
    static const std::regex numRegEx("[0-9]+");
    std::cmatch m;
    if (std::regex_search(pos, str.end(), m, identRegEx,
                          std::regex_constants::match_continuous)) {
      kind = Token::IDENT;
    } else if (std::regex_search(pos, str.end(), m, numRegEx,
                                 std::regex_constants::match_continuous)) {
      kind = Token::NUM;
    }
    if (kind != Token::EMPTY) {
      tok.str = std::string_view(m[0].first, m[0].second);
      tok.kind = kind;
      pos = m[0].second;
      return;
    }
    error("Invalid token");
  }

  Token::Kind eatSingleChar() {
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
    case '$':
      return Token::DOLLAR;
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
    default:
      return Token::EMPTY;
    }
  }

  Token peekToken() { return tok; }

  Token nextToken() {
    Token tmp = tok;
    getToken();
    return tmp;
  }

  bool matchPeekToken(Token::Kind kind) {
    if (tok.kind != kind)
      return false;

    return true;
  }

  bool matchNextToken(Token::Kind kind) {
    if (tok.kind != kind)
      return false;

    getToken();
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
      getToken();
      return true;
    }
    return false;
  }

  Token expectSSANum() {
    matchNextToken(Token::PERCENT);
    return expectNextToken(Token::IDENT);
  }

  Token expectPlaceholderNum() {
    matchNextToken(Token::HASH);
    return expectNextToken(Token::IDENT);
  }
};

class Record {
public:
  std::string_view name;
  std::string_view type;
  virtual ~Record() {}

  virtual void parse(Lexer &lex) {}
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
    while (!(lex.matchPeekToken(Token::CURLYC) ||
             lex.matchPeekToken(Token::EMPTY))) {
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
  std::vector<std::unique_ptr<Record>> records;
  void parse(Lexer &lex) override { records = Parser(lex, fac).parseRecords(); }
  RecordFactory &fac;
};

class SymbolTable {};

class RegRecord {};
class InstrRecord {};
class RegClassRecord {};
class ArchitectureRecord {
  std::vector<RegRecord> regs;
  std::vector<RegClassRecord> regClasses;
  std::vector<InstrRecord> instrs;
};

class DSLListRecord : public Record {
public:
  void parse(Lexer &lex) override {
    while (lex.matchPeekToken(Token::IDENT)) {
      refs.push_back(lex.nextToken());
    }
  }
  std::vector<std::string_view> refs;
};

class IRPatternRecord : public RecordSpace {

public:
  IRPatternRecord(RecordFactory &fac) : RecordSpace(fac) {}

  struct OperandPat {
    enum Kind {
      SSA_DEF,
      SSA_USE,
      PLACEHOLDER,
    };
    Kind kind;
    std::string_view name;
    std::string_view type;
  };

  struct InstrPat {
    std::string_view opcode;
    std::vector<OperandPat> operands;
    bool isGenerated = false;

    static InstrPat parse(Lexer &lex) {
      InstrPat instr;
      instr.opcode = lex.expectNextToken(Token::IDENT);
      while (!lex.matchNextToken(Token::SEMICOLON)) {
        instr.operands.emplace_back();
        OperandPat &op = instr.operands.back();
        if (lex.matchPeekToken(Token::PERCENT)) {
          op.kind = OperandPat::SSA_USE;
          op.name = lex.expectSSANum();
          continue;
        }
        if (lex.matchPeekToken(Token::IDENT)) {
          lex.expectNextIdent("def");
          op.kind = OperandPat::SSA_DEF;
          lex.expectNextToken(Token::PARENO);
          op.name = lex.expectSSANum();
          lex.expectNextToken(Token::COMMA);
          op.type = lex.expectNextToken(Token::IDENT);
          lex.expectNextToken(Token::PARENC);
          continue;
        }
        if (lex.matchPeekToken(Token::HASH)) {
          op.kind = OperandPat::PLACEHOLDER;
          op.name = lex.expectPlaceholderNum();
          continue;
        }
        error("Invalid operand pattern");
      }
      return instr;
    }
  };

  struct Match : public Record {
    std::vector<InstrPat> instrs;
    void parse(Lexer &lex) {
      while (!lex.matchPeekToken(Token::CURLYC)) {
        instrs.push_back(InstrPat::parse(lex));
      }
    }
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

  void genInstrMatch(InstrPat &instrPat, std::string_view instrVar) {
    instrPat.isGenerated = true;
    std::cout << std::format(
        "if({}.getKind() != Instr::INSTR_{}) return false;\n", instrVar,
        instrPat.opcode);
    unsigned opNum = 0;
    for (auto &op : instrPat.operands) {
      switch (op.kind) {
      case OperandPat::SSA_DEF:
        if (op.name != "_") {
          std::cout << std::format("auto &def_{} = {}.getOperand({});\n",
                                   op.name, instrVar, opNum);
        }
        if (op.type != "_") {
          std::cout << std::format(
              "if({}.getOperand({}).ssaDefType() != {}) return false;\n",
              instrVar, opNum, genType(op.type));
        }
        break;
      case OperandPat::SSA_USE: {
        if (op.name == "_")
          break;
        auto it = ssaDefs.find(op.name);
        if (it == ssaDefs.end()) {
          error("SSA Def undeclared");
        }
        auto &useInstrPat = *it->second;
        if (useInstrPat.isGenerated) {
          std::cout << std::format("if(&{}.getOperand({}).ssaUse().getDef() != "
                                   "&def_{}) return false;\n",
                                   instrVar, opNum, op.name);
        } else {
          std::cout << std::format(
              "auto &def_{}_instr = "
              "{}.getOperand({}).ssaUse().getDef().getParent();\n",
              op.name, instrVar, opNum);
          genInstrMatch(useInstrPat, std::format("def_{}_instr", op.name));
        }
        break;
      }
      case OperandPat::PLACEHOLDER:
        if (op.name == "_")
          break;
        std::cout << std::format("auto &ph_{} = {}.getOperand({});\n", op.name,
                                 instrVar, opNum);
        break;
      }
      ++opNum;
    }
  }

  void genInstrEmit() {

  }

  void collectSSADefs(Match &m) {
    for (auto &instr : m.instrs) {
      for (auto &op : instr.operands) {
        if (op.kind == OperandPat::SSA_DEF) {
          if (op.name == "_") {
            continue;
          }
          auto [_, succ] = ssaDefs.try_emplace(op.name, &instr);
          if (!succ) {
            error("SSA Def redeclared");
          }
        }
      }
    }
  }

  void gen() {
    Match &m = dynamic_cast<Match &>(*records[0]);
    collectSSADefs(m);
    genInstrMatch(m.instrs.back(), "root");
  }

  std::unordered_map<std::string_view, InstrPat *> ssaDefs;
};

int main(int argc, char *argv[]) {
  if (argc != 3) {
    std::cerr << "Expected filename" << std::endl;
    return EXIT_FAILURE;
  }
  std::ifstream f(argv[1], std::ios::ate);
  if (!f.good()) {
    std::cerr << "Couldn't open file" << std::endl;
    return EXIT_FAILURE;
  }

  auto sz = f.tellg();
  f.seekg(0);
  std::string str;
  str.resize(sz);
  f.read(str.data(), sz);

  RecordFactory fac;
  RecordFactory facIRPat;
  fac.registerRecord("IRPattern", [&]() {
    return std::make_unique<IRPatternRecord>(facIRPat);
  });
  fac.registerRecord("Arch",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("InstrSelector",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("dsl_list",
                     [&]() { return std::make_unique<DSLListRecord>(); });

  facIRPat.registerRecord(
      "match", []() { return std::make_unique<IRPatternRecord::Match>(); });
  facIRPat.registerRecord(
      "emit", []() { return std::make_unique<IRPatternRecord::Match>(); });

  Lexer lex(str);
  RecordSpace rs(fac);
  /*
  Token tok;
  while ((tok = lex.nextToken())) {
    std::cout << tok.str << " ";
  }
  */
  rs.parse(lex);
  for (auto &rec : rs.records) {
    if (rec->name == argv[2]) {
      IRPatternRecord &pat = dynamic_cast<IRPatternRecord &>(*rec);
      pat.gen();
    }
  }
}
