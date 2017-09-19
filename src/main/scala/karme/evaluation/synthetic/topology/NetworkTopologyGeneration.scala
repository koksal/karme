package karme.evaluation.synthetic.topology

import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph

trait NetworkTopologyGeneration {

  def generate(): NetworkTopologyGraph

}
