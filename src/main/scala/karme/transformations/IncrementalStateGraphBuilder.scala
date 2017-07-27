package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Reporter
import karme.analysis.DiscreteStateAnalysis
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.util.MathUtil
import karme.util.TimingUtil
import karme.visualization.StateGraphPlotter

class IncrementalStateGraphBuilder(
  exp: Experiment[Boolean],
  clustering: Map[String, Set[String]],
  trajectories: Seq[CellTrajectory]
) {

  val MAX_HAMMING_DISTANCE = 3

  val V = StateGraphs.nodesFromExperiment(exp)

  val nodePartialOrdering = new NodePartialOrderByPseudotimeRankSum(V.toSeq,
    trajectories).partialOrdering

  def buildGraph: DirectedBooleanStateGraph = {
    val connectedGraph = buildGraphFromEarliestNodes
    removeNodesWithoutNeighbors(connectedGraph)
  }

  private def removeNodesWithoutNeighbors(
    g: DirectedBooleanStateGraph
  ): DirectedBooleanStateGraph = {
    val nodesWithoutNeighbors = g.V filter { v =>
      g.undirectedNeighbors(v).isEmpty
    }

    var newG = g
    for (v <- nodesWithoutNeighbors) {
      newG = newG.removeVertex(v)
    }

    newG
  }

  private def buildGraphFromEarliestNodes: DirectedBooleanStateGraph = {
    var graph = new DirectedBooleanStateGraph()
    for (n <- nodesWithoutPredecessor(V)) {
      graph = graph.addVertex(n)
    }

    var keepSearching = true
    while (keepSearching) {
      val graphBeforeExtension = graph
      val minHammingNeighbors = chooseMinimalHammingNeighbors(graph.V)

      for ((source, neighbor) <- minHammingNeighbors) {
        graph = graph.addEdge(source, neighbor)
      }

      keepSearching = graph != graphBeforeExtension
    }

    graph
  }

  private def chooseMinimalHammingNeighbors(
    reachableNodes: Set[StateGraphVertex]
  ): Set[(StateGraphVertex, StateGraphVertex)] = {
    val distancesToNeighbors = hammingDistancesToTargets(reachableNodes, V)

    val validNeighbors = distancesToNeighbors filter {
      case (source, target, distance) => {
        distance < MAX_HAMMING_DISTANCE &&
          nodePartialOrdering.lt(source, target)
      }
    }

    val minDistance = validNeighbors.map(_._3).min
    val minDistanceNeighbors = validNeighbors.filter(_._3 == minDistance)
    minDistanceNeighbors.map(triple => (triple._1, triple._2))
  }

  private def hammingDistancesToTargets(
    sources: Set[StateGraphVertex],
    targets: Set[StateGraphVertex]
  ): Set[(StateGraphVertex, StateGraphVertex, Int)] = {
    MathUtil.cartesianProduct(List(sources, targets)) map {
      case List(source, target) => {
        (source, target,
          DiscreteStateAnalysis.hammingDistance(source.state, target.state))
      }
    }
  }

  def initialNodes(g: DirectedBooleanStateGraph): Set[StateGraphVertex] = {
    nodesWithoutPredecessor(g.V)
  }

  private def nodesWithoutPredecessor(
    vs: Set[StateGraphVertex]
  ) : Set[StateGraphVertex] = {
    vs filter { candidateV =>
      !vs.exists(otherV => nodePartialOrdering.lt(otherV, candidateV))
    }
  }

}
