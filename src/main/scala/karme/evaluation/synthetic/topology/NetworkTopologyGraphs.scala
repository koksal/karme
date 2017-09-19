package karme.evaluation.synthetic.topology

import karme.graphs.Graphs.{UnlabeledDiGraph, VertexLike}

object NetworkTopologyGraphs {
  case class NetworkTopologyGraphNode(id: String) extends VertexLike
  type NetworkTopologyGraph = UnlabeledDiGraph[NetworkTopologyGraphNode]
}
