package karme.graphs

import karme.graphs.Graphs.Graph

case class DiscreteStateGraph(
  V: Set[DiscreteStateGraphNode],
  E: Set[DiscreteStateGraphEdge],
  names: Seq[String]
) extends Graph {
  type Vertex = DiscreteStateGraphNode
  type Edge = DiscreteStateGraphEdge

  def addVertex(v: DiscreteStateGraphNode): DiscreteStateGraph = {
    this.copy(V = V + v)
  }

  def addEdge(
    v1: DiscreteStateGraphNode, v2: DiscreteStateGraphNode, label: String
  ): DiscreteStateGraph = {
    if (v1 < v2) {
      this.copy(E = E + DiscreteStateGraphEdge(v1, v2, label))
    } else {
      this.copy(E = E + DiscreteStateGraphEdge(v2, v1, label))
    }
  }
}

case class DiscreteStateGraphNode(state: Seq[Int])
  extends Ordered[DiscreteStateGraphNode] {
  override def compare(o: DiscreteStateGraphNode): Int = {
    assert(this.state.size == o.state.size)

    import scala.math.Ordering.Implicits._
    if (this.state < o.state) {
      -1
    } else if (this.state == o.state) {
      0
    } else {
      1
    }
  }
}
case class DiscreteStateGraphEdge(
  n1: DiscreteStateGraphNode,
  n2: DiscreteStateGraphNode,
  label: String
)
