package karme.evaluation.synthetic.topology

import karme.graphs.Graphs.UnlabeledDiGraph
import karme.graphs.Graphs.VertexLike

object NetworkTopologyGraphs {
  case class NetworkTopologyGraphNode(id: String) extends VertexLike
  type NetworkTopologyGraph = UnlabeledDiGraph[NetworkTopologyGraphNode]
}
