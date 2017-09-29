package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class BranchingNetworkGeneration(
  branchLength: Int
) extends NetworkTopologyGeneration {

  def generate(): NetworkTopologyGraph = {
    val prefix = makeNodeSeq(branchLength)
    val suffix1 = makeNodeSeq(branchLength)
    val suffix2 = makeNodeSeq(branchLength)

    var graph = LinearNetworkGeneration.makeGraphFromNodes(prefix)
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix1))
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix2))

    graph = graph.addEdge(prefix.last, suffix1.head)
    graph = graph.addEdge(prefix.last, suffix2.head)

    graph
  }

}
