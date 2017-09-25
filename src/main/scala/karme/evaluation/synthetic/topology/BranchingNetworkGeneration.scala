package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class BranchingNetworkGeneration(
  nbNodesPerBranch: Int
) extends NetworkTopologyGeneration {

  val linearNetworkGen = new LinearNetworkGeneration(nbNodesPerBranch)

  def generate(): NetworkTopologyGraph = {
    val prefix = linearNetworkGen.makeNodeSequence()
    val suffix1 = linearNetworkGen.makeNodeSequence()
    val suffix2 = linearNetworkGen.makeNodeSequence()

    var graph = LinearNetworkGeneration.makeGraphFromNodes(prefix)
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix1))
      .union(LinearNetworkGeneration.makeGraphFromNodes(suffix2))

    graph = graph.addEdge(prefix.last, suffix1.head)
    graph = graph.addEdge(prefix.last, suffix2.head)

    graph
  }

}
