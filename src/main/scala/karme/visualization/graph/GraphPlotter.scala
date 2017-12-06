package karme.visualization.graph

import java.io.File

import karme.Reporter
import karme.util.FileUtil

import scala.sys.process._
import scala.language.postfixOps

trait GraphPlotter {

  def reporter: Reporter

  def plotGraph(
    dotString: String,
    name: String
  ): Unit = {
    val dotFile = File.createTempFile("state-graph", ".dot")
    val pngFile = reporter.file(s"$name.png")
    FileUtil.writeToFile(dotFile, dotString)
    s"dot -Tpng ${dotFile.getAbsolutePath}" #> pngFile !
  }

  def dotGraph(
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

  def undirectedDotEdge(
    lhsID: String,
    rhsID: String,
    labels: Iterable[String],
    color: String = "black"
  ): String = {
    dotEdge(lhsID, rhsID, labels, false, color)
  }

  def directedDotEdge(
    lhsID: String,
    rhsID: String,
    labels: Iterable[String],
    color: String = "black"
  ): String = {
    dotEdge(lhsID, rhsID, labels, true, color)
  }

  private def dotEdge(
    lhsID: String,
    rhsID: String,
    labels: Iterable[String],
    isDirected: Boolean,
    color: String
  ): String = {
    val operator = if (isDirected) "->" else "--"
    s"""${lhsID} ${operator} ${rhsID} [label="${labels.mkString(",")}", color="${color}"]
       |""".stripMargin
  }

  def dotNode(
    id: String, label: String, color: String
  ): String = {
    s"""${id} [label="${label}", fillcolor="${color}", style="filled"];
       |""".stripMargin
  }

}
