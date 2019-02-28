set(LLVM_LINK_COMPONENTS support)

add_clang_executable(aster-tool
  ASTERTool.cpp
  )
target_link_libraries(aster-tool
  PRIVATE
  clangBasic
  clangFrontend
  clangTooling
  clangToolingASTDiff
  clangToolingMatcherGen
  clangDynamicASTMatchers
  )
