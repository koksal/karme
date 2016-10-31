package karme.graphs

object Graphs {
  abstract class Graph {
    type Vertex <: Ordered[Vertex]
    type Edge

    val V: Set[Vertex]
    val E: Set[Edge]
  }
}
