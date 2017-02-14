package karme.synthesis.traversal

import karme.synthesis.FunctionTrees.SymFunExpr
import karme.synthesis.FunctionTrees.SymFunLeaf
import karme.synthesis.FunctionTrees.SymFunTree
import karme.synthesis.Trees
import org.scalatest.FunSuite

class BFSTest extends FunSuite {

  val posVars = Set("a", "b", "c")

  test("BFS on a leaf") {
    val v = Trees.mkFreshIntVar("v")
    val symFunTree: SymFunExpr = new SymFunLeaf(v, posVars)

    val expected = Seq(symFunTree)

    assertResult(expected)(BFS.order(symFunTree))
  }

  test("BFS on a 2-deep tree") {
    val v1 = Trees.mkFreshIntVar("v1")
    val v2 = Trees.mkFreshIntVar("v2")
    val v3 = Trees.mkFreshIntVar("v3")
    val v4 = Trees.mkFreshIntVar("v4")

    val l1 = new SymFunLeaf(v1, posVars)
    val l2 = new SymFunLeaf(v2, posVars)
    val l3 = new SymFunLeaf(v3, posVars)
    val l4 = new SymFunLeaf(v4, posVars)

    val andSubtreeVar = Trees.mkFreshIntVar("andSubtree")
    val andSubtree = new SymFunTree(l1, andSubtreeVar, l2, posVars)
    val orSubtreeVar = Trees.mkFreshIntVar("orSubtree")
    val orSubtree = new SymFunTree(l3, orSubtreeVar, l4, posVars)

    val treeVar = Trees.mkFreshIntVar("tree")
    val tree = new SymFunTree(andSubtree, treeVar, orSubtree, posVars)

    val expectedOrder = Seq(tree, andSubtree, orSubtree, l1, l2, l3, l4)

    assertResult(expectedOrder)(BFS.order(tree))
  }


}
