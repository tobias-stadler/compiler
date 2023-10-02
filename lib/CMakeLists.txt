set(DSL_HDRS)
macro(dslgen arg_file arg_rec)
  add_custom_command(OUTPUT dsl/${arg_file}.h
    COMMAND dsl-cc
      ${CMAKE_CURRENT_SOURCE_DIR}/${arg_file}
      ${CMAKE_CURRENT_BINARY_DIR}/dsl/${arg_file}.h
      ${arg_rec}
      ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS dsl-cc ${arg_file}
    )
  list(APPEND DSL_HDRS dsl/${arg_file}.h)
endmacro()

dslgen(riscv/Arch.dsl riscv)
dslgen(riscv/InstrSelector.dsl isel)

add_library(core
c/Lexer.cpp
c/Parser.cpp
c/Type.cpp
c/IRGen.cpp
c/ASTPrinter.cpp
c/Semantics.cpp
ir/IR.cpp
ir/IRPrinter.cpp
ir/RegLiveness.cpp
ir/DominatorTree.cpp
riscv/InstrSelector.cpp
${DSL_HDRS}
)

target_compile_features(core PUBLIC cxx_std_20)
target_include_directories(core PUBLIC ../include)
target_include_directories(core PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/dsl)
