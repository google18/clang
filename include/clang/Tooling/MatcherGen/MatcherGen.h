#ifndef LLVM_CLANG_TOOLING_DIFFMATCHER_DIFFMATCHER_H
#define LLVM_CLANG_TOOLING_DIFFMATCHER_DIFFMATCHER_H

#include "clang/ASTMatchers/Dynamic/Registry.h"
#include "clang/Tooling/ASTDiff/ASTDiff.h"
#include "clang/Tooling/Tooling.h"

#include <string>
#include <vector>

namespace clang {
namespace matcher_gen {
  void printTree(const diff::SyntaxTree&);
  void printMatcher(const diff::SyntaxTree&,
                    const diff::NodeId&,
                    std::string&,
                    bool,
                    bool,
                    bool,
                    bool,
                    bool,
                    bool);
  void cleanUpCommas(std::string&);
  diff::NodeId LCA(const diff::SyntaxTree&, 
                   std::vector<diff::NodeId>);
  std::vector<diff::NodeId> findSourceDiff(const diff::SyntaxTree&,
                                           const diff::SyntaxTree&,
                                           const diff::ASTDiff&);
} // end namespace matcher_gen  
} // end namespace clang

#endif