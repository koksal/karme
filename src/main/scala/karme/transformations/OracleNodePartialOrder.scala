package karme.transformations

import karme.graphs.Graphs
import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.Forward
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex

class OracleNodePartialOrder(
  simulationGraph: DirectedBooleanStateGraph
) {

  def partialOrdering: PartialOrdering[StateGraphVertex] = {
    new PartialOrdering[StateGraphVertex] {

      override def tryCompare(
        x: StateGraphVertex,
        y: StateGraphVertex
      ): Option[Int] = {
        if (!edgeInGraph(x, y)) {
          None
        } else {
          if (this.lt(x, y)) {
            Some(-1)
          } else if (this.equiv(x, y)) {
            Some(0)
          } else {
            assert(this.gt(x, y))
            Some(1)
          }
        }
      }

      override def lteq(
        x: StateGraphVertex,
        y: StateGraphVertex
      ): Boolean = {
        directedEdgeExists(x, y)
      }
    }
  }

  private def directedEdgeExists(
    x: StateGraphVertex,
    y: StateGraphVertex
  ): Boolean = {
    getDirections(x, y).contains(Forward)
  }

  private def getDirections(
    x: StateGraphVertex,
    y: StateGraphVertex
  ): Set[EdgeDirection] = {
    if (!edgeInGraph(x, y)) {
      Set.empty
    } else {
      val e = Graphs.lexicographicEdge(x, y)
      val ds = simulationGraph.edgeDirections(e)

      if (isForwardQuery(x, y)) {
        ds
      } else {
        ds.map(Graphs.reverseDirection)
      }
    }
  }

  private def edgeInGraph(x: StateGraphVertex, y: StateGraphVertex) = {
    simulationGraph.E.contains(Graphs.lexicographicEdge(x, y))
  }

  private def isForwardQuery(
    x: StateGraphVertex,
    y: StateGraphVertex
  ): Boolean = {
    x < y
  }

}
