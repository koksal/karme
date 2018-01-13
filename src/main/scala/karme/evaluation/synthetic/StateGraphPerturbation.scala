package karme.evaluation.synthetic

import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.util.CollectionUtil

import scala.util.Random

object StateGraphPerturbation {

  def deleteNodes(
    stateGraph: DirectedBooleanStateGraph,
    nodeDeletionRatio: Double,
    random: Random
  ) = {
    var newGraph = stateGraph

    val nbNodesToDelete = (stateGraph.V.size * nodeDeletionRatio).toInt
    val nodesToDelete = CollectionUtil.randomElements(random)(stateGraph.V,
      nbNodesToDelete)

    for (nodeToDelete <- nodesToDelete) {
      newGraph = newGraph.removeVertex(nodeToDelete)
    }

    newGraph
  }

}
