set(LLVM_LINK_COMPONENTS support)

add_clang_executable(test-tool
  DiffTemplate.cpp
  )
target_link_libraries(test-tool
  PRIVATE
  clangBasic
  clangFrontend
  clangTooling
  clangToolingASTDiff
  clangToolingMatcherGen
  clangDynamicASTMatchers
  )
