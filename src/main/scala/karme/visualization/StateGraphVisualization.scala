package karme.visualization

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.Forward
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{DirectedStateGraph, DiscreteStateGraphEdge, DiscreteStateGraphNode, UndirectedStateGraph}
import karme.synthesis.Transitions.Transition
import karme.util.FileUtil

import scala.collection.mutable
import scala.language.postfixOps
import scala.sys.process._

object StateGraphVisualization {

  def plotUndirectedGraph(
    g: UndirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val nodeToID = makeNodeIDs(g.V)
    val dotString = undirectedDotString(g, clustering, nodeToID)
    plotGraph(dotString, "undirected-state-graph.png", outFolder)
    printCellsPerNodeID(nodeToID, outFolder)
  }

  def plotDirectedGraph(
    g: DirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val nodeToID = makeNodeIDs(g.V)
    val dotString = directedDotString(g, clustering, nodeToID)
    plotGraph(dotString, "directed-state-graph.png", outFolder)
    printCellsPerNodeID(nodeToID, outFolder)
  }

  def plotTransitions(
    g: DirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    transitions: Iterable[Transition],
    outFolder: File
  ): Unit = {
    // TODO augment directed plotting to superimpose transitions
  }

  private def plotGraph(
    dotString: String,
    fname: String,
    outFolder: File
  ): Unit = {
    val dotFile = new File(outFolder, "state-graph.dot")
    val pngFile = new File(outFolder, fname)
    FileUtil.writeToFile(dotFile, dotString)
    s"dot -Tpng ${dotFile.getAbsolutePath}" #> pngFile !
  }

  private def undirectedDotString(
    g: UndirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    nodeToID: Map[DiscreteStateGraphNode, String]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeToID)
    val edgeStr = undirectedDotEdges(g, nodeToID)
    dotGraph(nodeStr, edgeStr, isDirected = false)
  }

  private def directedDotString(
    g: DirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    nodeToID: Map[DiscreteStateGraphNode, String]
  ): String = {
    val nodeStr = dotNodes(g.V, clustering, nodeToID)
    val edgeStr = directedDotEdges(g, nodeToID)
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

  private def makeNodeIDs(
    vs: Iterable[DiscreteStateGraphNode]
  ): Map[DiscreteStateGraphNode, String] = {
    vs.toSeq.sorted.zipWithIndex.map{
      case (v, i) => {
        v -> s"V$i"
      }
    }.toMap
  }

  private def dotNodes(
    V: Iterable[DiscreteStateGraphNode],
    clustering: mutable.MultiMap[String, String],
    nodeToId: Map[DiscreteStateGraphNode, String]
  ): String = {
    val sb = new StringBuilder()
    for (node <- V) {
      val id = nodeToId(node)
      val clustersStr =
        StateGraphs.nodeMeasurementsPerCluster(node, clustering).map{
        case (cname, ms) => s"$cname (${ms.size})"
      }.mkString("{", ",", "}")
      val nodeStr = s"${id} ${clustersStr}"
      sb append (s"""${id} [label="${nodeStr}"];""")
      sb append "\n"
    }
    sb.toString()
  }

  private def undirectedDotEdges(
    g: UndirectedStateGraph,
    nodeToId: Map[DiscreteStateGraphNode, String]
  ): String = {
    val sb = new StringBuilder()
    for (e @ DiscreteStateGraphEdge(n1, n2) <- g.E) {
      val labels = g.edgeLabels(e)
      val lhsID = nodeToId(n1)
      val rhsID = nodeToId(n2)
      sb append s"${lhsID} -- ${rhsID}"
      sb append " [label=\""
      sb append labels.mkString(",")
      sb append "\"]\n"
    }

    sb.toString()
  }

  private def directedDotEdges(
    g: DirectedStateGraph,
    nodeToID: Map[DiscreteStateGraphNode, String]
  ): String = {
    val sb = new StringBuilder()
    for (e @ DiscreteStateGraphEdge(n1, n2) <- g.E) {
      val labels = g.edgeLabels(e)
      val lhsID = nodeToID(n1)
      val rhsID = nodeToID(n2)
      val edgeDirections = g.edgeDirections(e).toSet
      if (edgeDirections contains Forward) {
        sb append directedDotEdge(lhsID, rhsID, labels)
      }
      if (edgeDirections contains Backward) {
        sb append directedDotEdge(rhsID, lhsID, labels)
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

  private def printCellsPerNodeID(
    nodeToID: Map[DiscreteStateGraphNode, String],
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
