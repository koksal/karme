package karme.evaluation.synthetic.topology
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraphNode
import karme.graphs.Graphs.UnlabeledDiGraph

import scala.util.Random

class RandomGraphGeneration(nbNodes: Int) extends NetworkTopologyGeneration {

  private val random = new Random

  private val MIN_INDEGREE = 1
  private val MAX_INDEGREE = 3

  def generate(): NetworkTopologyGraph = {
    val nodeSeq = makeNodeSeq(nbNodes)

    var graph = UnlabeledDiGraph(V = nodeSeq.toSet)

    for (node <- nodeSeq) {
      // randomly pick number of inputs
      val indegree = makeRandomDegree()

      // randomly assign inputs
      val possibleInputs = nodeSeq.filter(_ != node)
      val inputs = chooseRandomNodes(possibleInputs, indegree)

      // add edges
      for (input <- inputs) {
        graph = graph.addEdge(input, node)
      }
    }

    graph
  }

  def makeRandomDegree(): Int = {
    random.nextInt(MAX_INDEGREE - MIN_INDEGREE + 1) + MIN_INDEGREE
  }

  def chooseRandomNodes(
    ns: Seq[NetworkTopologyGraphNode], size: Int
  ): Seq[NetworkTopologyGraphNode] = {
    random.shuffle(ns).take(size)
  }


}
