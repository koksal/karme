package karme.transformations

import karme.Reporter
import karme.analysis.DiscreteStateAnalysis
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.util.MathUtil
import karme.visualization.graph.StateGraphPlotter

class IncrementalStateGraphBuilder(
  V: Set[StateGraphVertex],
  nodePartialOrder: PartialOrdering[StateGraphVertex]
)(implicit reporter: Reporter) {

  val MAX_HAMMING_DISTANCE = 3
  val plotter = new StateGraphPlotter(reporter)

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
    var counter = 0
    do {
      prevGraph = graph
      counter += 1

      graph = saturateGraphWithAllMinimalOutgoingEdges(graph)
      plotter.plotDirectedGraph(graph, s"graph-$counter-a-saturation")

      graph = extendReachability(graph)
      plotter.plotDirectedGraph(graph, s"graph-$counter-b-extension")
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
    val minimalNeighbors = minimalHammingNeighbors(g.V, V, MAX_HAMMING_DISTANCE)

    var newGraph = g
    for ((source, target) <- minimalNeighbors) {
      newGraph = newGraph.addEdge(source, target)
    }
    newGraph
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
      case (source, target, _) => nodePartialOrder.lt(source, target)
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
      !vs.exists(otherV => nodePartialOrder.lt(otherV, candidateV))
    }
  }

}
