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

  def directedDotEdge(
    lhsID: String, rhsID: String, labels: Iterable[String]
  ): String = {
    s"""${lhsID} -> ${rhsID} [label="${labels.mkString(",")}"]
       |""".stripMargin
  }

  def dotNode(
    id: String, label: String, color: String
  ): String = {
    s"""${id} [label="${label}", fillcolor="${color}", style="filled"];
       |""".stripMargin
  }

}
