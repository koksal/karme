package karme.evaluation.synthetic.topology

import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.{NetworkTopologyGraph, NetworkTopologyGraphNode}
import karme.util.UniqueCounter

trait NetworkTopologyGeneration {

  private val counter = new UniqueCounter

  def generate(): NetworkTopologyGraph

  def makeNode(): NetworkTopologyGraphNode = {
    NetworkTopologyGraphNode(s"v${counter.next}")
  }

  def makeNodeSeq(length: Int): Seq[NetworkTopologyGraphNode] = {
    (1 to length) map (i => makeNode())
  }

}
