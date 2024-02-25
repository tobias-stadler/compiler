#include "IRPatRecord.h"

#include <cassert>
#include <format>
#include <regex>

IRPatternRecord::InstrPat IRPatternRecord::InstrPat::parse(Lexer &lex) {
  InstrPat instr;
  instr.opcode = lex.expectRecordIdent();
  while (!lex.matchOrEmptyNext(Token::SEMICOLON)) {
    instr.operands.emplace_back();
    OperandPat &op = instr.operands.back();
    if (lex.match(Token::PERCENT)) {
      op.kind = OperandPat::SSA_USE;
      op.name = lex.expectSSAName();
    } else if (lex.match(Token::IDENT)) {
      if (lex.matchIdentNext("def")) {
        lex.expectNext(Token::PARENO);
        if (lex.match(Token::PERCENT)) {
          op.kind = OperandPat::SSA_DEF;
          op.name = lex.expectSSAName();
          lex.expectNext(Token::COMMA);
          if (lex.match(Token::IDENT)) {
            op.type = lex.expectRecordIdent();
          } else {
            op.code = lex.expectCode();
          }
        } else {
          op.kind = OperandPat::RECIDENT_DEF;
          op.recIdent = lex.expectRecordIdent();
        }
        lex.expectNext(Token::PARENC);
      } else if (lex.matchIdentNext("eq")) {
        op.kind = OperandPat::COND_EQ;
      } else if (lex.matchIdentNext("ne")) {
        op.kind = OperandPat::COND_NE;
      } else if (lex.matchIdentNext("lt")) {
        op.kind = OperandPat::COND_LT;
      } else if (lex.matchIdentNext("ltu")) {
        op.kind = OperandPat::COND_LTU;
      } else if (lex.matchIdentNext("le")) {
        op.kind = OperandPat::COND_LE;
      } else if (lex.matchIdentNext("leu")) {
        op.kind = OperandPat::COND_LEU;
      } else if (lex.matchIdentNext("gt")) {
        op.kind = OperandPat::COND_GT;
      } else if (lex.matchIdentNext("gtu")) {
        op.kind = OperandPat::COND_GTU;
      } else if (lex.matchIdentNext("ge")) {
        op.kind = OperandPat::COND_GE;
      } else if (lex.matchIdentNext("geu")) {
        op.kind = OperandPat::COND_GEU;
      } else if (lex.matchIdentNext("imm32")) {
        op.kind = OperandPat::IMM32;
        lex.expectNext(Token::PARENO);
        if (lex.match(Token::IDENT)) {
          op.num = lex.expectNext(Token::IDENT);
        } else {
          op.code = lex.expectCode();
        }
        lex.expectNext(Token::PARENC);
      } else {
        op.kind = OperandPat::RECIDENT_USE;
        op.recIdent = lex.expectRecordIdent();
      }
    } else if (lex.match(Token::HASH)) {
      op.kind = OperandPat::OP_PLACEHOLDER;
      op.name = lex.expectPlaceholderName();
    } else {
      op.kind = OperandPat::RECIDENT_USE;
      op.recIdent = lex.expectRecordIdent();
    }
  }
  return instr;
}

std::pair<std::string, std::string>
IRPatternRecord::genType(RecordIdent &type) {
  Record *rec = parent->lookupIdent(type);
  if (type.idents.size() != 1) {
    error("Invalid type ident");
  }
  std::string_view tyStr = type.idents.back();
  static std::regex intRE("i([0-9]+)");
  std::cmatch m;
  if (std::regex_match(tyStr.begin(), tyStr.end(), m, intRE)) {
    return {"SSA_DEF_TYPE", std::format("IntSSAType::get({})", m[1].str())};
  }
  error("Invalid ir type");
}

void IRPatternRecord::genInstrMatch(InstrPat &instrPat,
                                    std::string_view instrVar,
                                    CodeBuilder &code) {
  if (instrPat.isGenerated) {
    return;
  }

  if (instrPat.opcode.idents.back() != "_") {
    code.println(std::format("if({}.getKind() != {}) return false;", instrVar,
                             instrPat.genInstrKind(*parent)));
  }
  instrPat.isGenerated = true;
  unsigned opNum = 0;
  for (auto &op : instrPat.operands) {
    switch (op.kind) {
    case OperandPat::SSA_DEF:
      if (op.name != "_") {
        code.println(std::format("auto &m_def_{} = {}.getOperand({});", op.name,
                                 instrVar, opNum));
      }
      if (!op.type.empty()) {
        code.println(std::format(
            "if({}.getOperand({}).ssaDefType() != {}) return false;", instrVar,
            opNum, genType(op.type).second));
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
              "auto &m_def_{} = {}.getOperand({}).ssaUse().getDef();", op.name,
              instrVar, opNum));
        } else if (usePat->isGenerated) {
          code.println(std::format("if(&{}.getOperand({}).ssaUse().getDef() != "
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
        genInstrMatch(*usePat, std::format("m_def_{}_instr", op.name), code);
      }
      break;
    }
    case OperandPat::OP_PLACEHOLDER:
      if (op.name == "_")
        break;
      code.println(std::format("auto &m_ph_{} = {}.getOperand({});", op.name,
                               instrVar, opNum));
      break;
    case OperandPat::COND_EQ:
    case OperandPat::COND_NE:
    case OperandPat::COND_LT:
    case OperandPat::COND_LTU:
    case OperandPat::COND_LE:
    case OperandPat::COND_LEU:
    case OperandPat::COND_GT:
    case OperandPat::COND_GTU:
    case OperandPat::COND_GE:
    case OperandPat::COND_GEU:
      code.println(std::format(
          "if({}.getOperand({}).brCond() != BrCond::{}()) return false;",
          instrVar, opNum, OperandPat::kindName(op.kind)));
      break;
    case OperandPat::IMM32:
      code.println(
          std::format("if({}.getOperand({}).imm32() != {}) return false;",
                      instrVar, opNum, op.num));
      break;
    case OperandPat::RECIDENT_DEF:
    case OperandPat::RECIDENT_USE:
      error("Unsupported operand pattern in match");
      break;
    }
    op.isGenerated = true;
    ++opNum;
  }
}

void IRPatternRecord::genEmit(InstrPats &pats, CodeBuilder &code) {
  unsigned instrNum = 0;
  code.println("auto& e_insertpoint = m_root.getNextNode();");
  for (auto &instr : pats.instrs) {
    code.println(std::format("Instr *e_instr_{} = new Instr({});", instrNum,
                             instr.genInstrKind(*parent)));
    code.println(
        std::format("e_insertpoint.insertPrev(e_instr_{});", instrNum));
    code.println(std::format("e_instr_{}->allocateOperands({});", instrNum,
                             instr.operands.size()));
    for (auto &op : instr.operands) {
      switch (op.kind) {
      case OperandPat::SSA_DEF: {
        std::string opTy, ty;
        if (op.code.size() > 0) {
          opTy = "SSA_DEF_TYPE";
          ty = genCode(op.code);
        } else {
          auto tyPair = genType(op.type);
          opTy = tyPair.first;
          ty = tyPair.second;
        }
        code.println(std::format("e_instr_{}->emplaceOperand<Operand::{}>({});",
                                 instrNum, opTy, ty));
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
        break;
      }
      case OperandPat::SSA_USE: {
        auto it = matchSSADefs.find(op.name);
        code.println(std::format(
            "e_instr_{}->emplaceOperand<Operand::SSA_USE>({}_def_{});",
            instrNum, it == matchSSADefs.end() ? "e" : "m", op.name));
        break;
      }
      case OperandPat::OP_PLACEHOLDER: {
        code.println(std::format(
            "e_instr_{}->emplaceOperand<Operand::EMPTY>();", instrNum));
        code.println(std::format("e_instr_{}->getLastOperand() = m_ph_{};",
                                 instrNum, op.name));
        break;
      }
      case OperandPat::IMM32: {
        code.println(std::format(
            "e_instr_{}->emplaceOperand<Operand::IMM32>({});", instrNum,
            op.code.size() > 0 ? genCode(op.code) : op.num));
        break;
      }
      case OperandPat::COND_EQ:
      case OperandPat::COND_NE:
      case OperandPat::COND_LT:
      case OperandPat::COND_LTU:
      case OperandPat::COND_LE:
      case OperandPat::COND_LEU:
      case OperandPat::COND_GT:
      case OperandPat::COND_GTU:
      case OperandPat::COND_GE:
      case OperandPat::COND_GEU: {
        code.println(std::format(
            "e_instr_{}->emplaceOperand<Operand::BRCOND>(BrCond::{}());",
            instrNum, OperandPat::kindName(op.kind)));
        break;
      }
      case OperandPat::RECIDENT_DEF:
        error("Unsupported operand pattern in emit");
        break;
      case OperandPat::RECIDENT_USE: {
        Record *rec = parent->lookupIdent(op.recIdent);
        if (!rec || rec->parent->type != "Arch" || rec->type != "Reg")
          error("Invalid recIdent");
        code.println(
            std::format("e_instr_{}->emplaceOperand<Operand::REG_USE>({}::{});",
                        instrNum, rec->parent->name, rec->name));
        break;
      }
      }
    }
    ++instrNum;
  }
}

void IRPatternRecord::parse(Lexer &lex) {
  while (!lex.matchOrEmpty(Token::CURLYC)) {
    std::string_view recType = lex.expectNext(Token::IDENT);
    lex.expectNext(Token::CURLYO);
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
    } else if (recType == "apply") {
      applyPats.emplace_back();
      applyPats.back().parse(lex);
    } else {
      error("Invalid operator in ir_pat");
    }
    lex.expectNext(Token::CURLYC);
  }
}
const char *IRPatternRecord::OperandPat::kindName(Kind kind) {
  switch (kind) {
  case SSA_DEF:
    return "def";
  case SSA_USE:
    return "%";
  case OP_PLACEHOLDER:
    return "#";
  case COND_EQ:
    return "eq";
  case COND_NE:
    return "ne";
  case COND_LT:
    return "lt";
  case COND_LTU:
    return "ltu";
  case COND_LE:
    return "le";
  case COND_LEU:
    return "leu";
  case COND_GT:
    return "gt";
  case COND_GTU:
    return "gtu";
  case COND_GE:
    return "ge";
  case COND_GEU:
    return "geu";
  case IMM32:
    return "imm32";
  case RECIDENT_DEF:
  case RECIDENT_USE:
    break;
  }
  return nullptr;
}
std::string IRPatternRecord::InstrPat::genInstrKind(RecordSpace &context) {
  Record *rec = context.lookupIdent(opcode);
  if (!rec || rec->type != "Instr" || rec->parent->type != "Arch") {
    return std::format("Instr::{}", opcode.getName());
  }
  return std::format("{}::{}", rec->parent->name, opcode.getName());
}

void IRPatternRecord::InstrPats::parse(Lexer &lex) {
  while (!lex.match(Token::CURLYC)) {
    instrs.push_back(InstrPat::parse(lex));
  }
}

void IRPatternRecord::genMatch(InstrPats &pats, CodeBuilder &code) {
  collectMatchSSADefs(pats);
  genInstrMatch(pats.instrs.back(), "m_root", code);
  for (auto &instr : pats.instrs) {
    if (!instr.isGenerated) {
      error("Ungenerated instruction pattern");
    }
    instr.isGenerated = false;
  }
}

void IRPatternRecord::genIf(IfPat &pat, CodeBuilder &code) {
  code.indent();
  code.print("if(!(");
  code.print(genCode(pat.code));
  code.print(")) return false;\n");
}

void IRPatternRecord::genApply(ApplyPat &pat, CodeBuilder &code) {
  code.indent();
  code.print(genCode(pat.code));
  code.print(";");
}

std::string IRPatternRecord::genCode(std::vector<Token> code) {
  std::string res;
  for (size_t i = 0; i < code.size(); ++i) {
    auto &tok = code[i];
    if (tok.kind == Token::CODE) {
      res += std::string_view(tok);
    } else if (tok.kind == Token::IDENT) {
      if (i == 0) {
        res += std::string_view(tok);
      } else
        switch (code[i - 1].kind) {
        default:
          res += std::string_view(tok);
          break;
        case Token::HASH:
          res += std::format("m_ph_{}", tok.str);
          break;
        case Token::PERCENT:
          res += std::format("m_def_{}", tok.str);
          break;
        }
    }
  }
  return res;
}

void IRPatternRecord::collectMatchSSADefs(InstrPats &m) {
  for (auto &instr : m.instrs) {
    unsigned opNum = 0;
    for (auto &op : instr.operands) {
      if ((op.kind == OperandPat::SSA_DEF || op.kind == OperandPat::SSA_USE) &&
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

void IRPatternRecord::gen(CodeBuilder &code) {
  genMatch(matchPats, code);
  if (matchPats.instrs.size() == 0) {
    error("[ir_pat] missing match pattern");
  }
  for (auto &pat : ifPats) {
    genIf(pat, code);
  }
  if (emitPats.instrs.size() > 0) {
    code.println();
    genEmit(emitPats, code);
  }
  for (auto &pat : applyPats) {
    genApply(pat, code);
  }
  code.println("return true;");
}
