#include "clang/Tooling/MatcherGen/MatcherGen.h"
#include <deque>
#include <algorithm>

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

namespace clang {
namespace matcher_gen {
// Convert an ASTNode to a corresponding node matcher
// Simplifies this by lowercasing the first character
// TODO: ObjC matchers and other edge cases
std::string toMatcherName(llvm::StringRef TypeLabel) {
  if (TypeLabel.startswith(StringRef("CXX"))) {
    return "cxx" + TypeLabel.drop_front(3).str();
  }
  else {
    return std::string(1, tolower(TypeLabel[0])) + TypeLabel.drop_front(1).str();
  }
}

std::string exprMatcher(const Expr* E) {
  std::string String;
  const clang::Type* TypePtr = E->getType().getTypePtr();
  if (TypePtr) {
    CXXRecordDecl* R = TypePtr->getAsCXXRecordDecl();
    if (R) {
      String += "hasType(cxxRecordDecl(hasName(\"";
      String += R->getNameAsString();
      String += "\"))), ";
    }
  }
  if (E->isInstantiationDependent()) {
    String += "isInstantiationDependent(), ";
  }
  if (E->isTypeDependent()) {
    String += "isTypeDependent(), ";
  }
  if (E->isValueDependent()) {
    String += "isValueDependent(), ";
  }
  return String;
}

// Creates matcher code for the arguments of a callExpr.
std::string callExprArgs(const CallExpr* CE) { 
  std::string MatchCode;
  CallExpr::const_arg_range Args = CE->arguments();
  size_t i = 0;
  for (const Expr* Arg : Args) {
    MatchCode += "hasArgument(" + std::to_string(i) + ", ";
    MatchCode += "expr()), "; // TODO: recurse here?
    ++i;
  }
  return MatchCode;
}

// Creates matcher code for the callee of a call expr.
std::string callExprCallee(const CallExpr* CE){
  std::string MatchCode;
  const clang::FunctionDecl* dirCallee = CE->getDirectCallee();
  if(dirCallee){
    MatchCode += "callee(";
    //might need to generalize
    MatchCode += "functionDecl(hasName(\"";
    MatchCode += dirCallee->getNameAsString()  + "\"))), ";
  }
  return MatchCode;
}

std::string nameMatcher(const NamedDecl* D) {
  std::string String;
  String += "hasName(\"";
  String += D->getNameAsString();
  String += "\"), ";
  return String;
}

std::string binOpMatcher(const BinaryOperator* B) {
  std::string String;
  String += "hasOperatorName(\"" + B->getOpcodeStr().str() + "\"), ";
  if (B->isAssignmentOp()) {
    String += "isAssignmentOp(), ";
  }
  String += "hasLHS(expr()), "; // TODO: recurse?
  String += "hasRHS(expr()), ";
  return String;
}

std::string parmMatcher(const ParmVarDecl* P) {
  std::string String;
  if (P -> hasDefaultArg()) {
    String += "hasDefaultArgument(), ";
  }
  return String;
}

std::string constructExprMatcher(const CXXConstructExpr* E) {
  std::string String;
  String += "argumentCountIs(";
  String += std::to_string(E->getNumArgs());
  String += "), ";
  if (E->isListInitialization()) {
    String += "isListInitialization(), ";
  }
  if (E->requiresZeroInitialization()) {
    String += "requiresZeroInitialization(), ";
  }
  return String;
}

std::string constructExprArgMatcher(const CXXConstructExpr* E) {
  std::string String;
  CXXConstructExpr::const_arg_range Args = E->arguments();
  size_t i = 0;
  for (const Expr* Arg : Args) {
    // TODO: recurse?
    String += "hasArgument(" + std::to_string(i) + ", expr()), ";
    ++i;
  }
  return String;
}

// Recursively print the matcher for a Tree at the
// given NodeId root
void printMatcher(const diff::SyntaxTree& Tree,
                  const diff::NodeId& Id,
                  std::string& Builder,
                  bool MatchExpr,
                  bool MatchBinOp,
                  bool MatchParmVarDecl,
                  bool MatchNamedDecl,
                  bool MatchConstructExpr,
                  bool MatchCallExpr) {

  // Get the Node object
  const diff::Node CurrNode = Tree.getNode(Id);

  // Get the ASTNode in the AST
  ast_type_traits::DynTypedNode ASTNode = CurrNode.ASTNode;
  
  // Simplest matcher for the node itself
  Builder += toMatcherName(CurrNode.getTypeLabel());
  Builder += "(";

  const Expr* E = ASTNode.get<Expr>();
  if (MatchExpr && E) {
    Builder += exprMatcher(E);
  }

  // TODO: ADD MORE NARROWING MATCHERS HERE

  const ImplicitCastExpr* Imp = ASTNode.get<ImplicitCastExpr>();
  if (Imp) {
    llvm::outs() << Imp->getType().getAsString() + "\n";
  }

  const BinaryOperator* B = ASTNode.get<BinaryOperator>();
  if (MatchBinOp && B) {
    Builder += binOpMatcher(B);
  }

  const ParmVarDecl* P = ASTNode.get<ParmVarDecl>();
  if (MatchParmVarDecl && P) {
    Builder += parmMatcher(P);
  }

  const NamedDecl* D = ASTNode.get<NamedDecl>();
  if (MatchNamedDecl && D) {
    Builder += nameMatcher(D);
  }

  const CXXConstructExpr* C = ASTNode.get<CXXConstructExpr>();
  if (MatchConstructExpr && C) {
    Builder += constructExprMatcher(C);
    Builder += constructExprArgMatcher(C);
  }

  const CallExpr* CE = ASTNode.get<CallExpr>();
  if (MatchCallExpr && CE) {
    Builder += callExprCallee(CE);
    Builder += callExprArgs(CE);
  }

  // Recurse through children
  // TODO: recurse only through some (?) children
  // i.e. let CallExpr, BinaryOperator handle their
  // children independently
  for (diff::NodeId Child : CurrNode.Children) {
    Builder += "has(";
    printMatcher(Tree, Child, Builder, MatchExpr, MatchBinOp, MatchParmVarDecl, MatchNamedDecl, MatchConstructExpr, MatchCallExpr);
    Builder += "), ";
  }

  Builder += ")";
}

// Removes malformed comma patterns in the resulting matcher
// string
void cleanUpCommas(std::string& String) {
  size_t Pos = std::string::npos;
  while ((Pos = String.find(", )")) != std::string::npos) {
    String.erase(Pos, 2);
  }
}

// Find root-to-node path for lowest common ancestor
std::deque<diff::NodeId> findRootPath(const diff::SyntaxTree& Tree, 
                                      const diff::NodeId& Id) {
  std::deque<diff::NodeId> Deque;
  diff::NodeId CurrId = Id;
  Deque.push_front(CurrId);
  while (CurrId != Tree.getRootId()) {
    CurrId = Tree.getNode(CurrId).Parent;
    Deque.push_front(CurrId);
  }
  return Deque;
}

// Lowest common ancestor for multiple nodes in AST
diff::NodeId LCA(const diff::SyntaxTree& Tree, 
                                     std::vector<diff::NodeId> Ids) {
  if (Ids.empty()) {
    llvm::outs() << "No AST difference found!\n";
    return diff::NodeId(-1);
  }

  std::vector<std::deque<diff::NodeId>> Paths;
  llvm::outs() << "Calculating root to node paths...\n";
  for (diff::NodeId Id : Ids) {
    Paths.push_back(findRootPath(Tree, Id));
  }
  
  for (std::deque<diff::NodeId> Path : Paths) {
    for (diff::NodeId Id : Path) {
      llvm::outs() << Id.Id << ", ";
    }
    llvm::outs() << "\n";
  }

  // LCA is bounded by length of shortest path
  size_t ShortestLength = Paths[0].size();
  for (size_t i = 0; i < Paths.size(); i++) {
    if (Paths[i].size() < ShortestLength) 
      ShortestLength = Paths[i].size();
  }

  llvm::outs() << "Iterating through paths...\n";

  // Iterate through paths until one differs
  size_t Idx;
  for (Idx = 0; Idx < ShortestLength; Idx++) {
    diff::NodeId CurrValue = Paths[0][Idx];
    for (std::deque<diff::NodeId> Path : Paths) {
      if (Path[Idx] != CurrValue) {
        return Path[Idx-1];
      } 
    }
  }
 
  return Paths[0][ShortestLength-1];
}

// Find highest but most specific ancestor of given node
// This is where we bind the root of our matcher
diff::NodeId walkUpNode(const diff::SyntaxTree& Tree,
                        const diff::NodeId Id) {
  diff::NodeId CurrId = Id;
  while (CurrId != Tree.getRootId()) {
    diff::NodeId Parent = Tree.getNode(CurrId).Parent;
    diff::Node ParentNode = Tree.getNode(Parent);
    llvm::StringRef ParentType = ParentNode.getTypeLabel();
    if (ParentType.equals(llvm::StringRef("DeclStmt")) || 
        ParentType.equals(llvm::StringRef("CompoundStmt")) ||
        ParentType.equals(llvm::StringRef("TranslationUnitDecl"))) {
      return CurrId;
    }
    CurrId = Parent;
  }
  return CurrId;
}

// Utility for computing a list of diffs with respect to the 
// source SyntaxTree
std::vector<diff::NodeId> findSourceDiff(const diff::SyntaxTree& SrcTree,
                                         const diff::SyntaxTree& DstTree,
                                         const diff::ASTDiff& Diff) {
  std::vector<diff::NodeId> DiffNodes;
  for (diff::NodeId Dst : DstTree) {
    const diff::Node &DstNode = DstTree.getNode(Dst);
    // Cover updates and update-moves
    if (DstNode.Change != diff::None && DstNode.Change != diff::Insert) {
      diff::NodeId Src = Diff.getMapped(DstTree, Dst);
      DiffNodes.push_back(Src);
    }
    // If insert, then the parent of insert is a diff in the source tree
    else if (DstNode.Change == diff::Insert) {
      diff::NodeId Src = Diff.getMapped(DstTree, DstNode.Parent);
      if (Src.isValid()) {
        DiffNodes.push_back(Src);
      }
    }
  }

  // Cover deletes
  // TODO: where to do moves?
  for (diff::NodeId Src : SrcTree) {
    if (Diff.getMapped(SrcTree, Src).isInvalid()) {
      DiffNodes.push_back(Src);
    }
  }
  return DiffNodes;
}

std::vector<matcher_gen::Diff> findSourceDiffList(const diff::SyntaxTree& SrcTree,
                                         const diff::SyntaxTree& DstTree,
                                         const diff::ASTDiff& Diff) {
  std::vector<matcher_gen::Diff> DiffNodes;
  for (diff::NodeId Dst : DstTree) {
    const diff::Node &DstNode = DstTree.getNode(Dst);
    // Cover updates and update-moves
    if (DstNode.Change != diff::Insert) {
      diff::NodeId Src = Diff.getMapped(DstTree, Dst);
      DiffNodes.push_back(matcher_gen::Diff::create(Src, Dst, DstNode.Change, SrcTree, DstTree));
    }
    // If insert, then the parent of insert is a diff in the source tree
    else if (DstNode.Change == diff::Insert) {
      diff::NodeId Src = Diff.getMapped(DstTree, DstNode.Parent);
      if (Src.isValid()) {
        DiffNodes.push_back(matcher_gen::Diff::create(Src, diff::NodeId(-1), diff::Insert, SrcTree, DstTree)); // TODO: How to encode this?
      }
    }
  }

  // Cover deletes
  // TODO: Where to do moves into new nodes in DstTree?
  for (diff::NodeId Src : SrcTree) {
    if (Diff.getMapped(SrcTree, Src).isInvalid()) {
      DiffNodes.push_back(matcher_gen::Diff::create(Src, diff::NodeId(-1), diff::Delete, SrcTree, DstTree)); // TODO: How to encode this?
    }
  }
  return DiffNodes;
}

std::vector<Diff> LCS(std::vector<Diff> D1, std::vector<Diff> D2) {
  int DP[D1.size() + 1][D2.size() + 1];
  for (size_t i = 0; i < D1.size() + 1; i++) {
    for (size_t j = 0; j < D2.size() + 1; j++) {
      if (i == 0 || j == 0) DP[i][j] = 0;
      else if (Diff::equivalent(D1[i-1], D2[j-1])) DP[i][j] = DP[i-1][j-1] + 1;
      else DP[i][j] = std::max(DP[i-1][j], DP[i][j-1]);
    }
  }

  int length = DP[D1.size()][D2.size()];

  std::vector<Diff> LCSDiff;
  int i = D1.size();
  int j = D2.size();

  while (i > 0 && j > 0) {
    if (Diff::equivalent(D1[i-1], D2[j-1])) {
      // Ignore TranslationUnitDecl which should be common among all ASTs
      if (D1[i-1].SLabel != "TranslationUnitDecl") { 
        LCSDiff.insert(LCSDiff.begin(), D1[i-1]); // Prioritize first list as source of truth
      }
      i--; j--; length--;
    }
    else if (DP[i-1][j] > DP[i][j-1]) i--;
    else j--;
  }

  return LCSDiff;
}

Diff Diff::create(diff::NodeId Src, diff::NodeId Dst, diff::ChangeKind ChangeKind, 
  const diff::SyntaxTree& SrcTree, const diff::SyntaxTree& DstTree) {
  std::string SrcLabel = SrcTree.getNode(Src).getTypeLabel().str();
  std::string DstLabel = (Dst.Id != -1) ? DstTree.getNode(Dst).getTypeLabel().str() : "";
  std::string SrcValue = SrcTree.getNodeValue(Src);
  std::string DstValue = (Dst.Id != -1) ? DstTree.getNodeValue(Dst) : "";
  return Diff(Src, Dst, ChangeKind, SrcLabel, SrcValue, DstLabel, DstValue);
}

// TODO: Make this more granular?
bool Diff::equivalent(Diff D1, Diff D2) {
  return D1.SrcTypeLabel == D2.SrcTypeLabel;
}

Diff::Diff(diff::NodeId Source, diff::NodeId Dest, diff::ChangeKind Kind, 
  std::string SLabel, std::string SVal, std::string DLabel, std::string DVal): 
  Src(Source), Dst(Dest), ChangeKind(Kind), SrcTypeLabel(SLabel), SrcValue(SVal), 
  DstTypeLabel(DLabel), DstValue(DVal) { }

raw_ostream& operator<<(raw_ostream& os, const Diff& D) {
  os << std::to_string(D.Src.Id);
  os << ": ";
  os << D.SrcTypeLabel;
  if (!D.SrcValue.empty()) {
    os << ": ";
    os << D.SrcValue;
  }
  os << ", ";
  os << std::to_string(D.Dst.Id);
  if (!D.DstTypeLabel.empty()) {
    os << ": ";
    os << D.DstTypeLabel;
  }
  if (!D.DstValue.empty()) {
    os << ": ";
    os << D.DstValue;
  }
  os << ", ";
  switch(D.ChangeKind) {
    case diff::None:
      os << "Match";
      break;
    case diff::Delete:
      os << "Delete";
      break;
    case diff::Update:
      os << "Update";
      break;
    case diff::Insert:
      os << "Insert";
      break;
    case diff::Move:
      os << "Move";
      break;
    case diff::UpdateMove:
      os << "Update and Move";
      break;
  }
  return os;
}

} // end namespace matcher_gen
} // end namespace clang
