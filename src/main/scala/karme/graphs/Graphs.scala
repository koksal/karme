package karme.graphs

import scala.collection.mutable

object Graphs {
  trait EdgeLike[Vertex] {
    def v1: Vertex
    def v2: Vertex
  }

  case class UnlabeledEdge[Vertex](v1: Vertex, v2: Vertex)
    extends EdgeLike[Vertex]
  case class LabeledEdge[Vertex, Label](v1: Vertex, v2: Vertex, label: Label)
    extends EdgeLike[Vertex]

  trait GraphLike[Vertex <: Ordered[Vertex], Edge <: EdgeLike[Vertex],
    G <: GraphLike[Vertex, Edge, G]] {
    def V: Set[Vertex]
    def E: Set[Edge]

    def neighbors(v: Vertex): Set[Vertex] = {
      var vs = Set[Vertex]()
      for (e <- E) {
        if (e.v1 == v) {
          vs += e.v2
        } else if (e.v2 == v) {
          vs += e.v1
        }
      }
      vs
    }

    def addVertex(v: Vertex): G
    def addEdge(v1: Vertex, v2: Vertex): G
  }

  trait DigraphLike[Vertex <: Ordered[Vertex], Edge <: EdgeLike[Vertex],
    G <: DigraphLike[Vertex, Edge, G]] {
    this: GraphLike[Vertex, Edge, G] =>

    def edgeDirections: mutable.MultiMap[Edge, EdgeDirection]

    override def neighbors(v: Vertex): Set[Vertex] = {
      var vs = Set[Vertex]()
      for (e <- E) {
        for (d <- edgeDirections(e)) {
          if (source(e, d) == v) {
            vs += target(e, d)
          }
        }
      }
      vs
    }

    def source(edge: Edge, direction: EdgeDirection): Vertex = direction match {
      case Forward => edge.v1
      case Backward => edge.v2
    }

    def target(edge: Edge, direction: EdgeDirection): Vertex = direction match {
      case Forward => edge.v2
      case Backward => edge.v1
    }
  }

  class UnlabeledGraph[Vertex <: Ordered[Vertex]](
    val V: Set[Vertex] = Set.empty,
    val E: Set[UnlabeledEdge[Vertex]] = Set.empty[UnlabeledEdge[Vertex]]
  ) extends GraphLike[Vertex, UnlabeledEdge[Vertex], UnlabeledGraph[Vertex]] {

    def addVertex(v: Vertex) = new UnlabeledGraph(V + v, E)

    def addEdge(v1: Vertex, v2: Vertex) = {
      val newEdge = if (v1 < v2) {
        UnlabeledEdge(v1, v2)
      } else {
        UnlabeledEdge(v2, v1)
      }
      new UnlabeledGraph(V + v1 + v2, E + newEdge)
    }
  }

  class UnlabeledDiGraph[Vertex <: Ordered[Vertex]](
    V: Set[Vertex] = Set.empty,
    E: Set[UnlabeledEdge[Vertex]] = Set.empty[UnlabeledEdge[Vertex]],
    val edgeDirections: mutable.MultiMap[UnlabeledEdge[Vertex], EdgeDirection]
  ) extends UnlabeledGraph(V, E)
    with GraphLike[Vertex, UnlabeledEdge[Vertex], UnlabeledGraph[Vertex]]
    with DigraphLike[Vertex, UnlabeledEdge[Vertex], UnlabeledGraph[Vertex]] {

    override def addEdge(v1: Vertex, v2: Vertex) = {
      val (newEdge, dir) = if (v1 < v2) {
        (UnlabeledEdge(v1, v2), Forward)
      } else {
        (UnlabeledEdge(v2, v1), Backward)
      }
      new UnlabeledDiGraph(
        V + v1 + v2,
        E + newEdge,
        edgeDirections.addBinding(newEdge, dir))
    }
  }

  sealed trait EdgeDirection
  case object Forward extends EdgeDirection
  case object Backward extends EdgeDirection
}
