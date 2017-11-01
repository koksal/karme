package karme.evaluation.synthetic

import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.util.CollectionUtil

object StateGraphPerturbation {

  def deleteNodes(
    stateGraph: DirectedBooleanStateGraph,
    nodeDeletionRatio: Double
  ) = {
    var newGraph = stateGraph

    val nbNodesToDelete = (stateGraph.V.size * nodeDeletionRatio).toInt
    val nodesToDelete = CollectionUtil.randomElements(stateGraph.V,
      nbNodesToDelete)

    for (nodeToDelete <- nodesToDelete) {
      newGraph = newGraph.removeVertex(nodeToDelete)
    }

    newGraph
  }

}
