package karme.evaluation.synthetic

import karme.analysis.DiscreteStateAnalysis
import karme.graphs.StateGraphs.DirectedBooleanStateGraph

class SimulationGraphAnalysis {

  def transitionToAll1HammingRatio(
    simulationGraph: DirectedBooleanStateGraph
  ): Double = {
    // find all 1-Hamming edges
    var nbHamming1Edges = 0
    val vertexList = simulationGraph.V.toVector
    for {
      i <- 0 until vertexList.size
      j <- (i + 1) until vertexList.size
    } {
      val hammingDist = DiscreteStateAnalysis.hammingDistance(
        vertexList(i).state, vertexList(j).state)

      if (hammingDist == 1) {
        nbHamming1Edges += 1
      }
    }

    simulationGraph.E.size.toDouble / nbHamming1Edges
  }

}
