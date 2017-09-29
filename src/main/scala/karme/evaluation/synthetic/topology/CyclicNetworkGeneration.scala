package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

class CyclicNetworkGeneration(size: Int) extends NetworkTopologyGeneration {

  def generate(): NetworkTopologyGraph = {
    val nodes = makeNodeSeq(size)

    var graph = LinearNetworkGeneration.makeGraphFromNodes(nodes)

    graph = graph.addEdge(nodes.last, nodes.head)

    graph
  }

}
