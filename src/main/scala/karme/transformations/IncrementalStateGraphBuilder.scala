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
  trajectories: Seq[CellTrajectory],
  distributionComparisonTest: DistributionComparisonTest
) {

  val MAX_HAMMING_DISTANCE = 3

  val V = StateGraphs.nodesFromExperiment(exp)

  val nodePartialOrdering = new NodePartialOrderByPseudotimeRankSum(V.toSeq,
    trajectories, distributionComparisonTest).partialOrdering

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

    var prevGraph = graph
    do {
      prevGraph = graph

      graph = saturateGraphWithAllMinimalOutgoingEdges(graph)
      graph = extendReachability(graph)
    } while (prevGraph != graph)

    graph
  }

  private def saturateGraphWithAllMinimalOutgoingEdges(
    g: DirectedBooleanStateGraph
  ): DirectedBooleanStateGraph = {
    var prevGraph = g
    var currGraph = g

    do {
      prevGraph = currGraph
      currGraph = extendAllNodesWithMinimalDistance(currGraph)
    } while (prevGraph != currGraph)

    currGraph
  }

  private def extendReachability(
    g: DirectedBooleanStateGraph
  ): DirectedBooleanStateGraph = {
    val edgesToNonReachedNodes = minimalHammingNeighbors(g.V, V -- g.V,
      MAX_HAMMING_DISTANCE)

    var newGraph = g
    for ((source, target) <- edgesToNonReachedNodes) {
      newGraph = newGraph.addEdge(source, target)
    }
    newGraph
  }

  private def extendAllNodesWithMinimalDistance(
    g: DirectedBooleanStateGraph
  ): DirectedBooleanStateGraph = {
    val minimalNeighbors = g.V flatMap { v =>
      minimalHammingNeighbors(Set(v), V, MAX_HAMMING_DISTANCE)
    }

    var newGraph = g
    for ((source, target) <- minimalNeighbors) {
      newGraph = newGraph.addEdge(source, target)
    }
    newGraph
  }

  private def allHammingNeighbors(
    reachableNodes: Set[StateGraphVertex],
    targetNodes: Set[StateGraphVertex],
    maxHammingDistance: Int
  ): Set[(StateGraphVertex, StateGraphVertex)] = {
    val edgesWithDistance = allHammingNeighborsWithDistance(reachableNodes,
      targetNodes, maxHammingDistance)

    edgesWithDistance map {
      case (source, target, _) => (source, target)
    }
  }

  private def minimalHammingNeighbors(
    reachableNodes: Set[StateGraphVertex],
    targetNodes: Set[StateGraphVertex],
    maxHammingDistance: Int
  ): Set[(StateGraphVertex, StateGraphVertex)] = {
    val allEdgesWithDistance = allHammingNeighborsWithDistance(reachableNodes,
      targetNodes, maxHammingDistance)

    if (allEdgesWithDistance.isEmpty) {
      Set.empty
    } else {
      val minDistance = allEdgesWithDistance.map(_._3).min
      val minDistanceNeighbors = allEdgesWithDistance.filter(_._3 == minDistance)

      minDistanceNeighbors.map {
        case (source, target, _) => (source, target)
      }
    }
  }

  private def allHammingNeighborsWithDistance(
    reachableNodes: Set[StateGraphVertex],
    targetNodes: Set[StateGraphVertex],
    maxHammingDistance: Int
  ): Set[(StateGraphVertex, StateGraphVertex, Int)] = {
    val edgesWithDistance = hammingDistancesToTargets(reachableNodes,
      targetNodes)

    val edgesWithinHammingBound = edgesWithDistance filter {
      case (source, target, distance) => distance <= maxHammingDistance
    }

    filterToPartialOrderValidEdges(edgesWithinHammingBound)
  }


  private def filterToPartialOrderValidEdges(
    edges: Set[(StateGraphVertex, StateGraphVertex, Int)]
  ): Set[(StateGraphVertex, StateGraphVertex, Int)] = {
    edges filter {
      case (source, target, _) => nodePartialOrdering.lt(source, target)
    }
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
