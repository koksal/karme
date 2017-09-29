package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.{NetworkTopologyGraph, NetworkTopologyGraphNode}
import karme.graphs.Graphs.UnlabeledDiGraph

class LinearNetworkGeneration(nbNodes: Int) extends NetworkTopologyGeneration {

  def generate(): NetworkTopologyGraph = {
    val nodeSeq = (1 to nbNodes).map(i => makeNode())
    LinearNetworkGeneration.makeGraphFromNodes(nodeSeq)
  }

}

object LinearNetworkGeneration {

  def makeGraphFromNodes(
    nodeSeq: Seq[NetworkTopologyGraphNode]
  ): NetworkTopologyGraph = {
    var graph = UnlabeledDiGraph(V = nodeSeq.toSet)

    for ((n1, n2) <- nodeSeq.zip(nodeSeq.tail)) {
      graph = graph.addEdge(n1, n2)
    }

    graph
  }

}
