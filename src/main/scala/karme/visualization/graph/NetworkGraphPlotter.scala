package karme.visualization.graph

import karme.Reporter
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr

class NetworkGraphPlotter(val reporter: Reporter) extends GraphPlotter {

  private val NODE_FILL_COLOR = "white"

  def plot(
    labelToFun: Map[String, FunExpr],
    name: String
  ): Unit = {
    plotGraph(graphDotString(labelToFun), name)
  }

  private def graphDotString(labelToFun: Map[String, FunExpr]): String = {
    dotGraph(
      dotNodes(labelToFun.keySet),
      dotEdges(labelToFun),
      isDirected = true
    )
  }

  private def dotNodes(labels: Set[String]): String = {
    labels.map(l => dotNode(l, l, NODE_FILL_COLOR)).mkString("")
  }

  private def dotEdges(labelToFun: Map[String, FunExpr]): String = {
    val sb = new StringBuilder

    for ((label, fun) <- labelToFun) {
      for ((inputLabel, appearsPositively) <-
           FunctionTrees.collectIdentifiersWithSigns(fun)) {
        val edgeLabel = if (appearsPositively) "+" else "-"
        sb append directedDotEdge(inputLabel, label, List(edgeLabel))
      }
    }

    sb.toString()
  }

}
