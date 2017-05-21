package karme.transformations

import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}

case class EdgePrecedence(source: String, target: String, distance: Int)

class EdgePrecedenceProducer {

  def computePrecedence(g: DirectedBooleanStateGraph): Seq[EdgePrecedence] = {
    g.V.toSeq.flatMap(v => computePrecedenceFromNode(g, v))
  }

  def computePrecedenceFromNode(
    g: DirectedBooleanStateGraph, v: StateGraphVertex
  ): Seq[EdgePrecedence] = {
    // compute shortest path to all other nodes, reconstruct precedence
    // information
    // do a BFS
    ???
  }

}
