package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.{NetworkTopologyGraph, NetworkTopologyGraphNode}
import karme.graphs.Graphs.UnlabeledDiGraph

class LinearNetworkGeneration(nbNodes: Int) extends NetworkTopologyGeneration {

  def generate(): NetworkTopologyGraph = {
    val nodes = makeNodes()

    var graph = UnlabeledDiGraph[NetworkTopologyGraphNode](V = nodes.toSet)

    for ((n1, n2) <- nodes.zip(nodes.tail)) {
      graph = graph.addEdge(n1, n2)
    }

    graph
  }

  private def makeNodes(): Seq[NetworkTopologyGraphNode] = {
    for (i <- 1 to nbNodes) yield {
      NetworkTopologyGraphNode(s"v$i")
    }
  }

}
