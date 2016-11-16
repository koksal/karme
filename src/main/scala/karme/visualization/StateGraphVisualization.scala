package karme.visualization

import java.io.File

import karme.Experiments.DiscreteExperiment
import karme.analysis.DiscreteStateAnalysis
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{DirectedStateGraph, DiscreteStateGraphEdge, DiscreteStateGraphNode, UndirectedStateGraph}
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
    val dotString = undirectedDotString(g, clustering)
    plotGraph(dotString, "undirected-state-graph.png", outFolder)
  }

  def plotDirectedGraph(
    g: DirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val dotString = ???
    plotGraph(dotString, "directed-state-graph.png", outFolder)
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
    g: UndirectedStateGraph, clustering: mutable.MultiMap[String, String]
  ): String = {
    // assign node IDs
    val nodeToID = g.V.zipWithIndex.map{
      case (v, i) => {
        v -> s"V$i"
      }
    }.toMap

    "graph G {\n" +
      "graph [layout=\"sfdp\", overlap=\"prism\"];\n" +
      dotNodes(g.V, clustering, nodeToID) + "\n" +
      dotEdges(g, nodeToID) + "\n" +
    "}"
  }

  private def directedDotString(
    g: DirectedStateGraph,
    clustering: mutable.MultiMap[String, String],
    nodeToID: Map[DiscreteStateGraphNode, String]
  ): String = {
    ???
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
      sb.append(id + " [label=\"" + clustersStr + "\"];\n")
    }
    sb.toString()
  }

  private def dotEdges(
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

}
