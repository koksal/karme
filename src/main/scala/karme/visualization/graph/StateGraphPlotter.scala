package karme.visualization.graph

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Reporter
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedBooleanStateGraph
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.Transition

class StateGraphPlotter(val reporter: Reporter) extends GraphPlotter {

  def plotUndirectedGraph(
    g: UndirectedBooleanStateGraph,
    name: String,
    cellClustering: Map[String, Set[String]] = Map.empty,
    nodeHighlightGroups: List[Set[ConcreteBooleanState]] = Nil,
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]] = Nil
  ): Unit = {
    val dotString = undirectedDotString(g, cellClustering,
      nodeHighlightGroups, edgeHighlightGroups)
    plotGraph(dotString, name)
  }

  def plotDirectedGraph(
    g: DirectedBooleanStateGraph,
    name: String,
    cellClustering: Map[String, Set[String]] = Map.empty,
    nodeHighlightGroups: List[Set[ConcreteBooleanState]] = Nil,
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]] = Nil
  ): Unit = {
    val dotString = directedDotString(g, cellClustering, nodeHighlightGroups,
      edgeHighlightGroups)
    plotGraph(dotString, name)
  }

  def plotTransitions(
    g: DirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    transitions: Iterable[Transition],
    name: String
  ): Unit = {
    val dotString = transitionDotString(g, clustering, transitions)
    plotGraph(dotString, name)
  }

  private def undirectedDotString(
    g: UndirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    nodeHighlightGroups: List[Set[ConcreteBooleanState]],
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeHighlightGroups)
    val edgeStr = undirectedDotEdges(g, edgeHighlightGroups)
    dotGraph(nodeStr, edgeStr, isDirected = false)
  }

  private def directedDotString(
    g: DirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    nodeHighlightGroups: List[Set[ConcreteBooleanState]],
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeHighlightGroups)
    val edgeStr = directedDotEdges(g, edgeHighlightGroups)
    dotGraph(nodeStr, edgeStr, isDirected = true)
  }

  private def transitionDotString(
    g: DirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    transitions: Iterable[Transition]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, Nil)
    val edgeStr = transitionDotEdges(g, transitions)
    dotGraph(nodeStr, edgeStr, isDirected = true)
  }

  private def dotNodes(
    V: Iterable[StateGraphVertex],
    clustering: Map[String, Set[String]],
    highlightGroups: List[Set[ConcreteBooleanState]]
  ): String = {
    val DEFAULT_COLOR = "white"
    val GROUP_COLORS = List("green", "yellow", "tomato")

    val sb = new StringBuilder()
    for (node <- V) {
      val highlightGroupIndex = highlightGroups.indexWhere { group =>
        group.contains(node.state)
      }
      val color = if (highlightGroupIndex < 0) {
        DEFAULT_COLOR
      } else {
        GROUP_COLORS(highlightGroupIndex)
      }

      val counts = node.measurements.size
      val clusterCountStrings =
        StateGraphs.nodeMeasurementsPerCluster(node, clustering).map{
        case (cname, ms) => s"$cname (${ms.size})"
      }
      val clusterString = if (clusterCountStrings.isEmpty) {
        ""
      } else {
        clusterCountStrings.mkString(" {", ", ", "}")
      }
      val nodeLabel = s"${node.id} / $counts${clusterString}"
      sb append dotNode(node.id, nodeLabel, color)
    }
    sb.toString()
  }

  private def undirectedDotEdges(
    g: UndirectedBooleanStateGraph,
    highlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]]
  ): String = {
    val DEFAULT_COLOR = "black"
    val GROUP_COLORS = List("green", "yellow", "tomato")

    val sb = new StringBuilder()
    for (e <- g.E) {
      val highlightGroupIndex = highlightGroups.indexWhere { group =>
        group.contains(e)
      }
      val color = if (highlightGroupIndex < 0) {
        DEFAULT_COLOR
      } else {
        GROUP_COLORS(highlightGroupIndex)
      }

      val labels = UndirectedStateGraphOps.edgeLabels(e)
      sb append undirectedDotEdge(e.v1.id, e.v2.id, labels, color)
    }

    sb.toString()
  }

  private def directedDotEdges(
    g: DirectedBooleanStateGraph,
    highlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]]
  ): String = {
    val DEFAULT_COLOR = "black"
    val GROUP_COLORS = List("green", "yellow", "tomato")

    val sb = new StringBuilder()
    for (e <- g.E) {
      val labels = UndirectedStateGraphOps.edgeLabels(e)
      val lhsID = e.v1.id
      val rhsID = e.v2.id
      val edgeDirections = g.edgeDirections.getOrElse(e, Set[EdgeDirection]())

      val highlightGroupIndex = highlightGroups.indexWhere { group =>
        group.contains(e)
      }
      val color = if (highlightGroupIndex < 0) {
        DEFAULT_COLOR
      } else {
        GROUP_COLORS(highlightGroupIndex)
      }

      if (edgeDirections contains Forward) {
        for (label <- labels) {
          val labelSuffix = if (e.v1.state.value(label)) "-" else "+"
          sb append directedDotEdge(lhsID, rhsID, Set(label + labelSuffix),
            color)
        }
      }
      if (edgeDirections contains Backward) {
        for (label <- labels) {
          val labelSuffix = if (e.v2.state.value(label)) "-" else "+"
          sb append directedDotEdge(rhsID, lhsID, Set(label + labelSuffix),
            color)
        }
      }
    }

    sb.toString()
  }

  private def transitionDotEdges(
    graph: DirectedBooleanStateGraph,
    transitions: Iterable[Transition]
  ): String = {
    val sb = new StringBuilder()

    for (transition <- transitions) {
      val inputVertex = graph.V.find(_.state == transition.input).get
      val neighbors = graph.targets(inputVertex)
      for (neighbor <- neighbors) {
        if (neighbor.state.value(transition.label) == transition.output) {
          val lhs = inputVertex.id
          val rhs = neighbor.id
          val label = s"${transition.label} (${transition.weight})"
          sb append directedDotEdge(lhs, rhs, List(label))
        }
      }
    }

    sb.toString()
  }

  private def printCellsPerNodeID(
    nodeToID: Map[StateGraphVertex, String],
    outFolder: File
  ): Unit = {
    val rows = nodeToID map {
      case (node, id) => {
        id +: node.measurements.map(_.id)
      }
    }

    val f = new File(outFolder, "node-id-to-cell.csv")
    val writer = CSVWriter.open(f)
    writer.writeAll(rows.toSeq)
  }
}
