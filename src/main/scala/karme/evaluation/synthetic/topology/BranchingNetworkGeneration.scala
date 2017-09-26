package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class BranchingNetworkGeneration(
  branchLength: Int
) extends NetworkTopologyGeneration {

  val linearNetworkGen = new LinearNetworkGeneration(0)

  def generate(): NetworkTopologyGraph = {
    val prefix = linearNetworkGen.makeNodeSequence(branchLength)
    val suffix1 = linearNetworkGen.makeNodeSequence(branchLength)
    val suffix2 = linearNetworkGen.makeNodeSequence(branchLength)

    var graph = LinearNetworkGeneration.makeGraphFromNodes(prefix)
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix1))
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix2))

    graph = graph.addEdge(prefix.last, suffix1.head)
    graph = graph.addEdge(prefix.last, suffix2.head)

    graph
  }

}
