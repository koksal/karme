package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class CyclicNetworkGeneration(size: Int) extends NetworkTopologyGeneration {

  private val linearNetworkGeneration = new LinearNetworkGeneration(0)

  def generate(): NetworkTopologyGraph = {
    val nodes = linearNetworkGeneration.makeNodeSequence(size)

    var graph = LinearNetworkGeneration.makeGraphFromNodes(nodes)

    graph = graph.addEdge(nodes.last, nodes.head)

    graph
  }

}
