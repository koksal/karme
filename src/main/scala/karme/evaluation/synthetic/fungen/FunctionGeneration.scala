package karme.evaluation.synthetic.fungen

import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph
import karme.synthesis.FunctionTrees.FunExpr

trait FunctionGeneration {

  def generate(topology: NetworkTopologyGraph): Map[String, FunExpr]

}
