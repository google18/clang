set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(clangToolingMatcherGen
  MatcherGen.cpp
  LINK_LIBS
  clangBasic
  clangAST
  clangLex
  )
