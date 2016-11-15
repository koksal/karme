package karme.graphs

import scala.collection.mutable

object Graphs {
  abstract class UndirectedGraph {
    type Vertex <: Ordered[Vertex]
    type Edge

    val V: Set[Vertex]
    val E: Set[Edge]
  }

  abstract class DirectedGraph extends UndirectedGraph {
    val edgeDirections: mutable.MultiMap[Edge, EdgeDirection]
  }

  sealed trait EdgeDirection
  case object Forward extends EdgeDirection
  case object Backward extends EdgeDirection
}
