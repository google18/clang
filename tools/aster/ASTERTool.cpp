#include "clang/ASTMatchers/Dynamic/Registry.h"
#include "clang/Tooling/ASTDiff/ASTDiff.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "clang/Tooling/MatcherGen/MatcherGen.h"
#include <vector>
#include <deque>
#include <sstream>
#include <fstream>

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

static cl::OptionCategory ClangDiffCategory("clang-diff options");

static cl::opt<std::string> SourcePath(cl::Positional, cl::desc("<source>"),
                                       cl::Required,
                                       cl::cat(ClangDiffCategory));

static cl::opt<std::string> DestinationPath(cl::Positional,
                                            cl::desc("<destination>"),
                                            cl::Optional,
                                            cl::cat(ClangDiffCategory));

static cl::opt<std::string> CompareFilesPath("m", cl::desc("TODO"), cl::init(""), 
                                             cl::Optional, cl::cat(ClangDiffCategory));

static cl::opt<std::string> BuildPath("p", cl::desc("Build path"), cl::init(""),
                                      cl::Optional, cl::cat(ClangDiffCategory));

static cl::opt<bool> MatchExpr("expr", cl::desc("Enable expressions"), cl::init(false));
static cl::opt<bool> MatchBinOp("binop", cl::desc("Enable binary operators"), cl::init(false));
static cl::opt<bool> MatchParmVarDecl("parmvardecl", cl::desc("Enable paramter variable declarations"), cl::init(false));
static cl::opt<bool> MatchCallExpr("callexpr", cl::desc("Enable calls"), cl::init(false));
static cl::opt<bool> MatchNamedDecl("nameddecl", cl::desc("Enable named declarations"), cl::init(false));
static cl::opt<bool> MatchConstructExpr("constructexpr", cl::desc("Enable construtor expressions"), cl::init(false));

static cl::list<std::string> ArgsAfter(
    "extra-arg",
    cl::desc("Additional argument to append to the compiler command line"),
    cl::cat(ClangDiffCategory));

static cl::list<std::string> ArgsBefore(
    "extra-arg-before",
    cl::desc("Additional argument to prepend to the compiler command line"),
    cl::cat(ClangDiffCategory));

static void addExtraArgs(std::unique_ptr<CompilationDatabase> &Compilations) {
  if (!Compilations)
    return;
  auto AdjustingCompilations =
      llvm::make_unique<ArgumentsAdjustingCompilations>(
          std::move(Compilations));
  AdjustingCompilations->appendArgumentsAdjuster(
      getInsertArgumentAdjuster(ArgsBefore, ArgumentInsertPosition::BEGIN));
  AdjustingCompilations->appendArgumentsAdjuster(
      getInsertArgumentAdjuster(ArgsAfter, ArgumentInsertPosition::END));
  Compilations = std::move(AdjustingCompilations);
}

static std::unique_ptr<ASTUnit>
getAST(const std::unique_ptr<CompilationDatabase> &CommonCompilations,
       const StringRef Filename) {
  std::string ErrorMessage;
  std::unique_ptr<CompilationDatabase> Compilations;
  if (!CommonCompilations) {
    Compilations = CompilationDatabase::autoDetectFromSource(
        BuildPath.empty() ? Filename : BuildPath, ErrorMessage);
    if (!Compilations) {
      llvm::errs()
          << "Error while trying to load a compilation database, running "
             "without flags.\n"
          << ErrorMessage;
      Compilations =
          llvm::make_unique<clang::tooling::FixedCompilationDatabase>(
              ".", std::vector<std::string>());
    }
  }
  addExtraArgs(Compilations);
  std::array<std::string, 1> Files = {{Filename}};
  ClangTool Tool(Compilations ? *Compilations : *CommonCompilations, Files);
  std::vector<std::unique_ptr<ASTUnit>> ASTs;
  Tool.buildASTs(ASTs);
  if (ASTs.size() != Files.size())
    return nullptr;
  return std::move(ASTs[0]);
}

static void printNode(raw_ostream &OS, diff::SyntaxTree &Tree,
                      diff::NodeId Id) {
  if (Id.isInvalid()) {
    OS << "None";
    return;
  }
  OS << Tree.getNode(Id).getTypeLabel();
  std::string Value = Tree.getNodeValue(Id);
  if (!Value.empty())
    OS << ": " << Value;
  OS << "(" << Id << ")";
}

static void printTree(raw_ostream &OS, diff::SyntaxTree &Tree) {
  for (diff::NodeId Id : Tree) {
    for (int I = 0; I < Tree.getNode(Id).Depth; ++I)
      OS << " ";
    printNode(OS, Tree, Id);
    OS << "\n";
  }
}

static void printDstChange(raw_ostream &OS, diff::ASTDiff &Diff,
                           diff::SyntaxTree &SrcTree, diff::SyntaxTree &DstTree,
                           diff::NodeId Dst) {
  const diff::Node &DstNode = DstTree.getNode(Dst);
  diff::NodeId Src = Diff.getMapped(DstTree, Dst);
  switch (DstNode.Change) {
  case diff::None:
    break;
  case diff::Delete:
    llvm_unreachable("The destination tree can't have deletions.");
  case diff::Update:
    OS << "Update ";
    printNode(OS, SrcTree, Src);
    OS << " to " << DstTree.getNodeValue(Dst) << "\n";
    break;
  case diff::Insert:
  case diff::Move:
  case diff::UpdateMove:
    if (DstNode.Change == diff::Insert)
      OS << "Insert";
    else if (DstNode.Change == diff::Move)
      OS << "Move";
    else if (DstNode.Change == diff::UpdateMove)
      OS << "Update and Move";
    OS << " ";
    printNode(OS, DstTree, Dst);
    OS << " into ";
    printNode(OS, DstTree, DstNode.Parent);
    OS << " at " << DstTree.findPositionInParent(Dst) << "\n";
    break;
  }
}

int main(int argc, const char **argv) {
  std::string ErrorMessage;
  std::unique_ptr<CompilationDatabase> CommonCompilations =
      FixedCompilationDatabase::loadFromCommandLine(argc, argv, ErrorMessage);
  if (!CommonCompilations && !ErrorMessage.empty())
    llvm::errs() << ErrorMessage;
  cl::HideUnrelatedOptions(ClangDiffCategory);
  if (!cl::ParseCommandLineOptions(argc, argv)) {
    cl::PrintOptionValues();
    return 1;
  }

  addExtraArgs(CommonCompilations);

  if (DestinationPath.empty()) {
    llvm::errs() << "Error: Exactly two paths are required.\n";
    return 1;
  }

  std::vector<std::pair<std::string, std::string>> ExampleFilenames;

  if (!CompareFilesPath.empty()) {
    std::ifstream InputStream(CompareFilesPath);
    std::stringstream StringStream;
    StringStream << InputStream.rdbuf();
    std::string Contents = StringStream.str();

    size_t Pos = 0;
    std::string Delimiter = "\n";
    while ((Pos = Contents.find(Delimiter)) != std::string::npos) {
      std::string Token = Contents.substr(0, Pos);
      size_t CommaPos = Token.find(",");
      if (CommaPos == std::string::npos) {
        llvm::outs() << "Malformed example filename list!";
        break;
      }
      std::string Before = Token.substr(0, CommaPos);
      std::string After = Token.substr(CommaPos + 1, Token.length());
      std::pair<std::string, std::string> Pair(Before, After);
      ExampleFilenames.push_back(Pair);
      Contents.erase(0, Pos + Delimiter.length());
    }

    for (std::pair<std::string, std::string> Elem : ExampleFilenames) {
      llvm::outs() << "(" << Elem.first << ", " << Elem.second << "), ";
    }
    llvm::outs() << "\n";
  }
  else {
    std::pair<std::string, std::string> Pair(SourcePath, DestinationPath);
    ExampleFilenames.push_back(Pair);
  }

  std::vector<std::vector<matcher_gen::Diff>> ListOfDiffs;

  for (std::pair<std::string, std::string> Pair : ExampleFilenames) {
    std::unique_ptr<ASTUnit> Src = getAST(CommonCompilations, Pair.first);
    std::unique_ptr<ASTUnit> Dst = getAST(CommonCompilations, Pair.second);
    if (!Src || !Dst) 
      return 1;

    diff::ComparisonOptions Options;

    diff::SyntaxTree SrcTree(Src->getASTContext());
    diff::SyntaxTree DstTree(Dst->getASTContext());
    diff::ASTDiff Diff(SrcTree, DstTree, Options); 
    
    llvm::outs() << "Before:\n";
    printTree(llvm::outs(), SrcTree);
    llvm::outs() << "\n";
    llvm::outs() << "After:\n";
    printTree(llvm::outs(), DstTree);
    llvm::outs() << "\n";

    llvm::outs() << "clang-diff:\n";
    for (diff::NodeId Dst : DstTree) {
      printDstChange(llvm::outs(), Diff, SrcTree, DstTree, Dst);
    }
    for (diff::NodeId Src : SrcTree) {
      if (Diff.getMapped(SrcTree, Src).isInvalid()) {
        llvm::outs() << "Delete ";
        printNode(llvm::outs(), SrcTree, Src);
        llvm::outs() << "\n";
      }
    }
    llvm::outs() << "\n";
   
    llvm::outs() << "Nodes of interest:\n";
    std::vector<diff::NodeId> DiffNodes = matcher_gen::findSourceDiff(SrcTree, DstTree, Diff);
    std::vector<matcher_gen::Diff> DiffList = matcher_gen::findSourceDiffList(SrcTree, DstTree, Diff);
    ListOfDiffs.push_back(DiffList);
    for (diff::NodeId Id : DiffNodes) {
      llvm::outs() << Id.Id << ", ";
    }
    llvm::outs() << "\n";

    llvm::outs() << "Computing LCA...\n";
    diff::NodeId Ancestor = matcher_gen::LCA(SrcTree, DiffNodes);
    if (Ancestor.Id != -1) {
      llvm::outs() << Ancestor.Id << "\n";
      llvm::outs() << "\n";
      std::string MatcherString;
      matcher_gen::printMatcher(SrcTree, Ancestor, MatcherString, MatchExpr, MatchBinOp, MatchParmVarDecl, MatchNamedDecl, MatchConstructExpr, MatchCallExpr);
      matcher_gen::cleanUpCommas(MatcherString);
      llvm::outs() << MatcherString << "\n";
      llvm::outs() << "\n";
    }
  }

  for (std::vector<matcher_gen::Diff> Diffs : ListOfDiffs) {
    for (matcher_gen::Diff Diff: Diffs) {
      llvm::outs() << Diff;
      llvm::outs() << "\n";
    }
    llvm::outs() << "\n\n";
  }

  if (ListOfDiffs.size() > 1) {
    std::vector<matcher_gen::Diff> D1 = ListOfDiffs[0];
    std::vector<matcher_gen::Diff> D2 = ListOfDiffs[1];
    std::vector<matcher_gen::Diff> LCS = matcher_gen::LCS(D1, D2);
    for (size_t i = 2; i < ListOfDiffs.size(); i++) {
      LCS = matcher_gen::LCS(LCS, ListOfDiffs[i]);
    }

    for (matcher_gen::Diff Diff : LCS) {
      llvm::outs() << Diff << "\n";
    }
    llvm::outs() << "\n";
  } 
  return 0;
}
