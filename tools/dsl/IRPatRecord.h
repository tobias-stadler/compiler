#pragma once

#include "CodeBuilder.h"
#include "Record.h"

class IRPatternRecord : public Record {

public:
  IRPatternRecord() {}

  struct OperandPat {
    enum Kind {
      SSA_DEF,
      SSA_USE,
      RECIDENT_DEF,
      RECIDENT_USE,
      OP_PLACEHOLDER,
      IMM32,
      COND_EQ,
      COND_NE,
      COND_LT,
      COND_LTU,
      COND_LE,
      COND_LEU,
      COND_GT,
      COND_GTU,
      COND_GE,
      COND_GEU,
    };

    static const char *kindName(Kind kind);

    Kind kind;
    std::string_view name;
    RecordIdent type;
    std::vector<Token> code;
    std::string_view num;
    RecordIdent recIdent;
    bool isGenerated = false;
  };

  struct InstrPat {
    static InstrPat parse(Lexer &lex);
    std::string genInstrKind(RecordSpace &context);

    RecordIdent opcode;
    std::vector<OperandPat> operands;
    bool isGenerated = false;
  };

  struct InstrPats {
    void parse(Lexer &lex);

    std::vector<InstrPat> instrs;
  };

  struct IfPat {
    void parse(Lexer &lex) { code = lex.expectCode(); }

    std::vector<Token> code;
  };

  struct ApplyPat {
    void parse(Lexer &lex) { code = lex.expectCode(); }

    std::vector<Token> code;
  };

  std::pair<std::string, std::string> genType(RecordIdent &type);
  void genInstrMatch(InstrPat &instrPat, std::string_view instrVar,
                     CodeBuilder &code);
  void genMatch(InstrPats &pats, CodeBuilder &code);
  void genIf(IfPat &pat, CodeBuilder &code);
  void genApply(ApplyPat &pat, CodeBuilder &code);
  void genEmit(InstrPats &pats, CodeBuilder &code);
  std::string genCode(std::vector<Token> code);
  void collectMatchSSADefs(InstrPats &m);
  void parse(Lexer &lex) override;
  void gen(CodeBuilder &code);

  std::unordered_map<std::string_view, std::pair<InstrPat *, unsigned>>
      matchSSADefs;

  InstrPats matchPats;
  InstrPats emitPats;
  std::vector<IfPat> ifPats;
  std::vector<ApplyPat> applyPats;
};
