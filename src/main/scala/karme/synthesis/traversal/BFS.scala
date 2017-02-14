package karme.synthesis.traversal

import karme.synthesis.FunctionTrees.SymFunExpr
import karme.synthesis.FunctionTrees.SymFunLeaf
import karme.synthesis.FunctionTrees.SymFunTree

import scala.collection.immutable.Queue

object BFS {
  def order(symFunExpr: SymFunExpr): Seq[SymFunExpr] = {
    var traversalQ = Queue.empty[SymFunExpr]
    var resultQ = Queue.empty[SymFunExpr]

    traversalQ = traversalQ.enqueue(symFunExpr)
    while (traversalQ.nonEmpty) {
      val (nextElem, updatedQ) = traversalQ.dequeue
      traversalQ = updatedQ
      resultQ = resultQ.enqueue(nextElem)

      // enqueue the children of the element being visited
      nextElem match {
        case SymFunTree(l, v, r) => {
          traversalQ = traversalQ.enqueue(l)
          traversalQ = traversalQ.enqueue(r)
        }
        case SymFunLeaf(_) => // do not enqueue anything
      }
    }

    resultQ
  }
}
