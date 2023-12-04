#include <format>
#include <fstream>
#include <iostream>

#include "CodeBuilder.h"
#include "IRPatRecord.h"
#include "Lexer.h"
#include "Record.h"

void genKindSwitch(std::string_view fnName, std::vector<RecordSpace *> &rs,
                   CodeBuilder &code) {
  code.startFunction(
      std::format("constexpr const char* {}(unsigned kind)", fnName));
  code.startBlock("switch(kind)");
  for (auto rec : rs) {
    code.println(std::format("case {}:", rec->name));
    code.println(std::format("return \"{}\";", rec->name));
  }
  code.endBlock();
  code.println("return nullptr;");
  code.endBlock();
}

void genArch(RecordSpace &rs, CodeBuilder &code) {
  std::vector<RecordSpace *> regRecs;
  std::vector<RecordSpace *> regClassRecs;
  std::vector<RecordSpace *> instrRecs;
  rs.gather(regRecs, "Reg");
  rs.gather(regClassRecs, "RegClass");
  rs.gather(instrRecs, "Instr");

  code.startBlock("enum ArchRegKind");
  code.println("ARCH_REG_START,");
  for (auto rec : regRecs) {
    code.println(std::format("{},", rec->name));
  }
  code.println("ARCH_REG_END,");
  code.endBlockSemicolon();
  code.startBlock("inline const ArchReg archRegs[] =");
  for (auto rec : regRecs) {
    auto *noLiveness = dynamic_cast<BoolRecord *>(rec->getRecord("noLiveness"));
    code.println(std::format(
        "{{ .reg = {}, .name = \"{}\", .noLiveness = {}}},", rec->name,
        rec->name, noLiveness ? noLiveness->gen() : "false"));
  }
  code.endBlockSemicolon();
  code.startFunction("constexpr const ArchReg* getArchReg(unsigned kind)");
  code.println("return kind > ARCH_REG_START && kind < ARCH_REG_END ? archRegs "
               "+ (kind - ARCH_REG_START - 1)"
               ": nullptr;");
  code.endBlock();

  code.startBlock("enum ArchInstrKind");
  code.println("ARCH_INSTR_START = Instr::ARCH_INSTR,");
  for (auto rec : instrRecs) {
    code.println(std::format("{},", rec->name));
  }
  code.println("ARCH_INSTR_END");
  code.endBlockSemicolon();
  code.startBlock("inline const ArchInstr archInstrs[] =");
  for (auto rec : instrRecs) {
    code.println(
        std::format("{{.kind = {}, .name = \"{}\"}},", rec->name, rec->name));
  }
  code.endBlockSemicolon();
  code.startFunction("constexpr const ArchInstr* getArchInstr(unsigned kind)");
  code.println(
      "return kind > ARCH_INSTR_START && kind < ARCH_INSTR_END ? archInstrs "
      "+ (kind - ARCH_INSTR_START - 1)"
      " : nullptr;");
  code.endBlock();

  code.startBlock("enum ArchRegClassKind");
  code.println("ARCH_REGCLASS_START,");
  for (auto rec : regClassRecs) {
    code.println(std::format("{},", rec->name));
  }
  code.println("ARCH_REGCLASS_END");
  code.endBlockSemicolon();
  for (auto rec : regClassRecs) {
    code.startBlock(std::format("inline const ArchReg *archRegClass{}Regs[] = ",
                                rec->name));
    auto regsRec = rec->getRecord("regs");
    if (!regsRec || regsRec->type != "dsl_list")
      error("RegClass missing regs property");
    for (auto regRec : dynamic_cast<DSLListRecord &>(*regsRec).genRecs()) {
      code.println(std::format("getArchReg({}),", regRec->name));
    }
    code.endBlockSemicolon();
  }
  code.startBlock("inline const ArchRegClass archRegClasses[] =");
  for (auto rec : regClassRecs) {
    code.println(
        std::format("{{.kind = {}, .name = \"{}\"}},", rec->name, rec->name));
  }
  code.endBlockSemicolon();
  code.startFunction(
      "constexpr const ArchRegClass* getArchRegClass(unsigned kind)");
  code.println("return kind > ARCH_REGCLASS_START && kind < ARCH_REGCLASS_END "
               "? archRegClasses "
               "+ (kind - ARCH_REGCLASS_START - 1)"
               " : nullptr;");
  code.endBlock();
}

void genIRPatExecutor(RecordSpace &rs, CodeBuilder &code) {
  std::vector<IRPatternRecord *> recs;
  rs.gatherRecursively(recs, "ir_pat");
  std::stable_sort(
      recs.begin(), recs.end(), [](IRPatternRecord *a, IRPatternRecord *b) {
        if (a->matchPats.instrs.back().opcode.getName() ==
            b->matchPats.instrs.back().opcode.getName()) {
          return a->matchPats.instrs.size() > b->matchPats.instrs.size();
        }
        return a->matchPats.instrs.back().opcode.getName() <
               b->matchPats.instrs.back().opcode.getName();
      });
  int numSameOpc = 0;
  std::string_view lastOpc;
  for (auto *rec : recs) {
    std::string_view opcName = rec->matchPats.instrs.back().opcode.getName();
    if (opcName == lastOpc) {
      ++numSameOpc;
    } else {
      numSameOpc = 0;
      lastOpc = opcName;
    }
    code.startFunction(
        std::format("bool dslPat{}{}(Instr &m_root)", opcName, numSameOpc));
    rec->gen(code);
    code.endBlock();
  }
  numSameOpc = 0;
  lastOpc = std::string_view();
  code.startFunction("bool dslExecutePat(Instr &m_root)");
  code.startBlock("switch(m_root.getKind())");
  code.println("default:");
  for (auto *rec : recs) {
    std::string_view opcName = rec->matchPats.instrs.back().opcode.getName();
    if (opcName == lastOpc) {
      ++numSameOpc;
    } else {
      code.println("break;");
      code.println(
          std::format("case {}: ",
                      rec->matchPats.instrs.back().genInstrKind(*rec->parent)));
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
  std::string includeDir = argv[4];

  RecordFactory fac;
  fac.registerRecord("ir_pat",
                     [&]() { return std::make_unique<IRPatternRecord>(); });
  fac.registerRecord("Arch",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("IRPatExecutor",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("dsl_list",
                     [&]() { return std::make_unique<DSLListRecord>(); });
  fac.registerRecord("Reg",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("Instr",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("RegClass",
                     [&]() { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("include", [&] {
    return std::make_unique<IncludeRecord>(fac, includeDir);
  });
  fac.registerRecord("using",
                     [&] { return std::make_unique<DSLListRecord>(); });
  fac.registerRecord("Template",
                     [&] { return std::make_unique<TemplateRecord>(); });
  fac.registerRecord("token", [] {
    auto rec = std::make_unique<TokenRecord>();
    rec->realType = "token";
    return rec;
  });
  fac.registerRecord("CommuteToken",
                     [&] { return std::make_unique<RecordSpace>(fac); });
  fac.registerRecord("bool", [] { return std::make_unique<BoolRecord>(); });

  IncludeRecord rs(fac, includeDir);
  rs.content = loadFile(argv[1]);
  rs.parseContent();

  CodeBuilder code;

  std::string_view genRecName = argv[3];
  auto *genRec = rs.getRecord(genRecName);
  if (!genRec) {
    error("Can't find this record");
  }

  rs.materializeTemplates();

  if (genRec->type == "Arch") {
    genArch(dynamic_cast<RecordSpace &>(*genRec), code);
  } else if (genRec->type == "IRPatExecutor") {
    genIRPatExecutor(dynamic_cast<RecordSpace &>(*genRec), code);
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
