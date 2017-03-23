package karme.visualization

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.Reporter
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedBooleanStateGraph
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.Transition
import karme.util.FileUtil
import karme.util.MapUtil

import scala.collection.mutable
import scala.language.postfixOps
import scala.sys.process._

class StateGraphPlotter(reporter: Reporter) {

  def plotUndirectedGraph(
    g: UndirectedBooleanStateGraph,
    name: String,
    cellClustering: mutable.MultiMap[String, String] = MapUtil.emptyMultiMap,
    nodeHighlightGroups: List[Set[ConcreteBooleanState]] = Nil,
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]] = Nil
  ): Unit = {
    val nodeToID = StateGraphs.makeNodeIDs(g.V.toSeq.sorted)
    val dotString = undirectedDotString(g, cellClustering, nodeToID,
      nodeHighlightGroups)
    plotGraph(dotString, name)
  }

  def plotDirectedGraph(
    g: DirectedBooleanStateGraph,
    name: String,
    cellClustering: mutable.MultiMap[String, String] = MapUtil.emptyMultiMap,
    nodeHighlightGroups: List[Set[ConcreteBooleanState]] = Nil,
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]] = Nil
  ): Unit = {
    val nodeToID = StateGraphs.makeNodeIDs(g.V.toSeq.sorted)
    val dotString = directedDotString(g, cellClustering, nodeToID,
      nodeHighlightGroups)
    plotGraph(dotString, name)
  }

  def plotTransitions(
    g: DirectedBooleanStateGraph,
    clustering: mutable.MultiMap[String, String],
    transitions: Iterable[Transition],
    nodeToID: Map[StateGraphVertex, String],
    name: String
  ): Unit = {
    val dotString = transitionDotString(g, clustering, transitions, nodeToID)
    plotGraph(dotString, name)
  }

  private def plotGraph(
    dotString: String,
    name: String
  ): Unit = {
    val dotFile = File.createTempFile("state-graph", ".dot")
    val pngFile = reporter.file(s"$name.png")
    FileUtil.writeToFile(dotFile, dotString)
    s"dot -Tpng ${dotFile.getAbsolutePath}" #> pngFile !
  }

  private def undirectedDotString(
    g: UndirectedBooleanStateGraph,
    clustering: mutable.MultiMap[String, String],
    nodeToID: Map[StateGraphVertex, String],
    nodeHighlightGroups: List[Set[ConcreteBooleanState]]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeToID, nodeHighlightGroups)
    val edgeStr = undirectedDotEdges(g, nodeToID)
    dotGraph(nodeStr, edgeStr, isDirected = false)
  }

  private def directedDotString(
    g: DirectedBooleanStateGraph,
    clustering: mutable.MultiMap[String, String],
    nodeToID: Map[StateGraphVertex, String],
    highlightGroups: List[Set[ConcreteBooleanState]]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeToID, highlightGroups)
    val edgeStr = directedDotEdges(g, nodeToID)
    dotGraph(nodeStr, edgeStr, isDirected = true)
  }

  private def transitionDotString(
    g: DirectedBooleanStateGraph,
    clustering: mutable.MultiMap[String, String],
    transitions: Iterable[Transition],
    nodeToID: Map[StateGraphVertex, String]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeToID, Nil)
    val edgeStr = transitionDotEdges(g, transitions, nodeToID)
    dotGraph(nodeStr, edgeStr, isDirected = true)
  }

  private def dotGraph(
    nodeStr: String,
    edgeStr: String,
    isDirected: Boolean
  ): String = {
    val graphDeclaration = if (isDirected) "digraph" else "graph"
    s"""${graphDeclaration} G {
       |graph [layout="neato", overlap="scale"];
       |${nodeStr}
       |${edgeStr}
       |}
       |""".stripMargin

  }

  private def dotNodes(
    V: Iterable[StateGraphVertex],
    clustering: mutable.MultiMap[String, String],
    nodeToId: Map[StateGraphVertex, String],
    highlightGroups: List[Set[ConcreteBooleanState]]
  ): String = {
    val DEFAULT_BACKGROUND_COLOR = "white"
    val GROUP_COLORS = List("green", "yellow", "tomato")

    val sb = new StringBuilder()
    for (node <- V) {
      val highlightGroupIndex = highlightGroups.indexWhere { group =>
        group.contains(node.state)
      }
      val color = if (highlightGroupIndex < 0) {
        DEFAULT_BACKGROUND_COLOR
      } else {
        GROUP_COLORS(highlightGroupIndex)
      }

      val id = nodeToId(node)
      val clustersStr =
        StateGraphs.nodeMeasurementsPerCluster(node, clustering).map{
        case (cname, ms) => s"$cname (${ms.size})"
      }.mkString("{", ",", "}")
      val nodeStr = s"${id} ${clustersStr}"
      sb append (
        s"""${id} [label="${nodeStr}", fillcolor="${color}", style="filled"];
           |""".stripMargin)
      sb append "\n"
    }
    sb.toString()
  }

  private def undirectedDotEdges(
    g: UndirectedBooleanStateGraph,
    nodeToId: Map[StateGraphVertex, String]
  ): String = {
    val sb = new StringBuilder()
    for (e <- g.E) {
      val labels = UndirectedStateGraphOps.edgeLabels(e)
      val lhsID = nodeToId(e.v1)
      val rhsID = nodeToId(e.v2)
      sb append s"${lhsID} -- ${rhsID}"
      sb append " [label=\""
      sb append labels.mkString(",")
      sb append "\"]\n"
    }

    sb.toString()
  }

  private def directedDotEdges(
    g: DirectedBooleanStateGraph,
    nodeToID: Map[StateGraphVertex, String]
  ): String = {
    val sb = new StringBuilder()
    for (e <- g.E) {
      val labels = UndirectedStateGraphOps.edgeLabels(e)
      val lhsID = nodeToID(e.v1)
      val rhsID = nodeToID(e.v2)
      val edgeDirections = g.edgeDirections(e).toSet
      if (edgeDirections contains Forward) {
        for (label <- labels) {
          val labelSuffix = if (e.v1.state.value(label)) "-" else "+"
          sb append directedDotEdge(lhsID, rhsID, Set(label + labelSuffix))
        }
      }
      if (edgeDirections contains Backward) {
        for (label <- labels) {
          val labelSuffix = if (e.v2.state.value(label)) "-" else "+"
          sb append directedDotEdge(rhsID, lhsID, Set(label + labelSuffix))
        }
      }
    }

    sb.toString()
  }

  private def directedDotEdge(
    lhsID: String, rhsID: String, labels: Iterable[String]
  ): String = {
    s"""${lhsID} -> ${rhsID} [label="${labels.mkString(",")}"]
       |""".stripMargin
  }

  private def transitionDotEdges(
    graph: DirectedBooleanStateGraph,
    transitions: Iterable[Transition],
    nodeToID: Map[StateGraphVertex, String]
  ): String = {
    val sb = new StringBuilder()

    for (transition <- transitions) {
      val inputVertex = graph.V.find(_.state == transition.input).get
      val neighbors = graph.neighbors(inputVertex)
      for (neighbor <- neighbors) {
        if (neighbor.state.value(transition.label) == transition.output) {
          val lhs = nodeToID(inputVertex)
          val rhs = nodeToID(neighbor)
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
