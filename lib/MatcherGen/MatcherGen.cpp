#include "clang/Tooling/MatcherGen/MatcherGen.h"
#include <deque>

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

void printTreeRecursive(const diff::SyntaxTree& Tree, 
                        const diff::NodeId CurrId,
                        size_t Level) {
  for (size_t i = 0; i < Level; i++) {
    llvm::outs() << "-";
  }
  const diff::Node CurrNode = Tree.getNode(CurrId);
  llvm::outs() << CurrNode.getTypeLabel();
  std::string Value = Tree.getNodeValue(CurrId);
  if (!Value.empty())
    llvm::outs() << ": " << Value;
  llvm::outs() << " (" << CurrId << ")\n";
  for (diff::NodeId Child : CurrNode.Children) {
    printTreeRecursive(Tree, Child, Level + 1);
  }
}

void printTree(const diff::SyntaxTree& Tree) {
  printTreeRecursive(Tree, Tree.getRootId(), 0);
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

} // end namespace matcher_gen
} // end namespace clang
