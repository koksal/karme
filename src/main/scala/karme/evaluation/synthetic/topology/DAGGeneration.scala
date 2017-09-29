package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class DAGGeneration(
  linearPathLen: Int
) extends NetworkTopologyGeneration {

  def generate(): NetworkTopologyGraph = {
    val prefix = makeNodeSeq(linearPathLen)
    val suffix = makeNodeSeq(linearPathLen)

    val leftDiamondHalf = makeNodeSeq(1)
    val rightDiamondHalf = makeNodeSeq(1)

    var graph = LinearNetworkGeneration.makeGraphFromNodes(prefix)
      .union(LinearNetworkGeneration.makeGraphFromNodes(leftDiamondHalf))
      .union(LinearNetworkGeneration.makeGraphFromNodes(rightDiamondHalf))
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix))

    graph = graph.addEdge(prefix.last, leftDiamondHalf.head)
    graph = graph.addEdge(prefix.last, rightDiamondHalf.head)

    graph = graph.addEdge(leftDiamondHalf.last, suffix.head)
    graph = graph.addEdge(rightDiamondHalf.last, suffix.head)

    graph
  }

}
