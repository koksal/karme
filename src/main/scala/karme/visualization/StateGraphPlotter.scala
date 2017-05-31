package karme.visualization

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.{Experiments, Reporter}
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
import karme.util.FileUtil

import scala.language.postfixOps
import scala.sys.process._

class StateGraphPlotter(reporter: Reporter) {

  def plotUndirectedGraph(
    g: UndirectedBooleanStateGraph,
    name: String,
    cellClustering: Map[String, Set[String]] = Map.empty,
    nodeHighlightGroups: List[Set[ConcreteBooleanState]] = Nil,
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]] = Nil
  ): Unit = {
    val dotString = undirectedDotString(g, cellClustering, nodeHighlightGroups)
    plotGraph(dotString, name)
  }

  def plotDirectedGraph(
    g: DirectedBooleanStateGraph,
    name: String,
    cellClustering: Map[String, Set[String]] = Map.empty,
    nodeHighlightGroups: List[Set[ConcreteBooleanState]] = Nil,
    edgeHighlightGroups: List[Set[UnlabeledEdge[StateGraphVertex]]] = Nil
  ): Unit = {
    val dotString = directedDotString(g, cellClustering, nodeHighlightGroups)
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
    clustering: Map[String, Set[String]],
    nodeHighlightGroups: List[Set[ConcreteBooleanState]]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeHighlightGroups)
    val edgeStr = undirectedDotEdges(g)
    dotGraph(nodeStr, edgeStr, isDirected = false)
  }

  private def directedDotString(
    g: DirectedBooleanStateGraph,
    clustering: Map[String, Set[String]],
    highlightGroups: List[Set[ConcreteBooleanState]]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, highlightGroups)
    val edgeStr = directedDotEdges(g)
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
    clustering: Map[String, Set[String]],
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
      val nodeStr = s"${node.id} / $counts${clusterString}"
      sb append (
        s"""${node.id} [label="${nodeStr}", fillcolor="${color}",
           |style="filled"];
           |""".stripMargin)
      sb append "\n"
    }
    sb.toString()
  }

  private def undirectedDotEdges(
    g: UndirectedBooleanStateGraph
  ): String = {
    val sb = new StringBuilder()
    for (e <- g.E) {
      val labels = UndirectedStateGraphOps.edgeLabels(e)
      sb append s"${e.v1.id} -- ${e.v2.id}"
      sb append " [label=\""
      sb append labels.mkString(",")
      sb append "\"]\n"
    }

    sb.toString()
  }

  private def directedDotEdges(
    g: DirectedBooleanStateGraph
  ): String = {
    val sb = new StringBuilder()
    for (e <- g.E) {
      val labels = UndirectedStateGraphOps.edgeLabels(e)
      val lhsID = e.v1.id
      val rhsID = e.v2.id
      val edgeDirections = g.edgeDirections.getOrElse(e, Set[EdgeDirection]())
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
