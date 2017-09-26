package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class DAGGeneration(
  linearPathLen: Int
) extends NetworkTopologyGeneration {

  val linearNetworkGen = new LinearNetworkGeneration(0)

  def generate(): NetworkTopologyGraph = {
    val prefix = linearNetworkGen.makeNodeSequence(linearPathLen)
    val suffix = linearNetworkGen.makeNodeSequence(linearPathLen)

    val leftDiamondHalf = linearNetworkGen.makeNodeSequence(1)
    val rightDiamondHalf = linearNetworkGen.makeNodeSequence(1)

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
