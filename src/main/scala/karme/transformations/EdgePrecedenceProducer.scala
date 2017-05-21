package karme.transformations

import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}

case class EdgePrecedence(source: String, target: String, distance: Int)

class EdgePrecedenceProducer {

  def computePrecedence(g: DirectedBooleanStateGraph): Seq[EdgePrecedence] = {
    g.V.toSeq.flatMap(v => computePrecedenceFromNode(g, v))
  }

  def computePrecedenceFromNode(
    g: DirectedBooleanStateGraph, v: StateGraphVertex
  ): Seq[EdgePrecedence] = {
    val shortestPaths = g.shortestPaths(v)
    val nonTrivialPaths = shortestPaths.filter(_.size > 1)

    nonTrivialPaths.toSeq flatMap precedenceFromPath
  }

  def precedenceFromPath(p: Seq[StateGraphVertex]): Seq[EdgePrecedence] = {
    require(p.size >= 2)

    val distance = p.size - 1
    val firstEdge = UnlabeledEdge(p(0), p(1))
    val lastEdge = UnlabeledEdge(p(p.size - 2), p(p.size - 1))

    for {
      l1 <- UndirectedStateGraphOps.edgeLabels(firstEdge)
      l2 <- UndirectedStateGraphOps.edgeLabels(lastEdge)
    } yield {
      EdgePrecedence(l1, l2, distance)
    }
  }

}
