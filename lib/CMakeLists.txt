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
)

target_compile_features(core PUBLIC cxx_std_20)
target_include_directories(core PUBLIC ../include)
