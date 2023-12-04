#pragma once

#include <sstream>
#include <string_view>

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
