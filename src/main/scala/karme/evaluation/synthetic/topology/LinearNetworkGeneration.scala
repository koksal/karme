package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.{NetworkTopologyGraph, NetworkTopologyGraphNode}
import karme.graphs.Graphs.UnlabeledDiGraph
import karme.util.UniqueCounter

class LinearNetworkGeneration(nbNodes: Int) extends NetworkTopologyGeneration {

  private val counter = new UniqueCounter

  def generate(): NetworkTopologyGraph = {
    LinearNetworkGeneration.makeGraphFromNodes(makeNodeSequence())
  }

  def makeNodeSequence(): Seq[NetworkTopologyGraphNode] = {
    for (i <- 1 to nbNodes) yield {
      NetworkTopologyGraphNode(s"v${counter.next}")
    }
  }

}

object LinearNetworkGeneration {

  def makeGraphFromNodes(
    nodeSeq: Seq[NetworkTopologyGraphNode]
  ): NetworkTopologyGraph = {
    var graph = UnlabeledDiGraph[NetworkTopologyGraphNode](V = nodeSeq.toSet)

    for ((n1, n2) <- nodeSeq.zip(nodeSeq.tail)) {
      graph = graph.addEdge(n1, n2)
    }

    graph
  }

}
