#ifndef LLVM_CLANG_TOOLING_DIFFMATCHER_DIFFMATCHER_H
#define LLVM_CLANG_TOOLING_DIFFMATCHER_DIFFMATCHER_H

#include "clang/ASTMatchers/Dynamic/Registry.h"
#include "clang/Tooling/ASTDiff/ASTDiff.h"
#include "clang/Tooling/Tooling.h"

#include <string>
#include <vector>

namespace clang {
namespace matcher_gen {
  class Diff {
    public:
      diff::NodeId Src;
      diff::NodeId Dst;
      diff::ChangeKind ChangeKind;
      std::string SrcTypeLabel;
      std::string SrcValue;
      std::string DstTypeLabel;
      std::string DstValue;

      static Diff create(diff::NodeId Src, diff::NodeId Dst, diff::ChangeKind ChangeKind, 
        const diff::SyntaxTree& SrcTree, const diff::SyntaxTree& DstTree);

      static bool equivalent(Diff D1, Diff D2);

      friend raw_ostream& operator<<(raw_ostream& os, const Diff& D);

    private:
      Diff(diff::NodeId Src, diff::NodeId Dst, diff::ChangeKind Kind, 
        std::string SLabel, std::string SVal, std::string DLabel, std::string DVal);
  };

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
  std::vector<Diff> findSourceDiffList(const diff::SyntaxTree&,
                                           const diff::SyntaxTree&,
                                           const diff::ASTDiff&);
  std::vector<Diff> LCS(std::vector<Diff>, std::vector<Diff>);
} // end namespace matcher_gen  
} // end namespace clang

#endif