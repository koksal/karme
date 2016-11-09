package karme.visualization

import java.io.File

import karme.Experiments.DiscreteExperiment
import karme.analysis.DiscreteStateAnalysis
import karme.util.FileUtil

import scala.collection.mutable
import scala.language.postfixOps
import scala.sys.process._

object DiscreteStateGraphVisualization {

  def plot(
    exp: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String],
    outFolder: File
  ): Unit = {
    val dotFile = new File(outFolder, "state-graph.dot")
    val pngFile = new File(outFolder, "state-graph.png")
    FileUtil.writeToFile(dotFile, dotString(exp, clustering))
    s"dot -Tpng ${dotFile.getAbsolutePath}" #> pngFile !
  }

  private def dotString(
    exp: DiscreteExperiment, clustering: mutable.MultiMap[String, String]
  ): String = {
    // find out unique states
    val stateToMeasurements = exp.measurements.groupBy(_.values)
    val uniqueStates = stateToMeasurements.keySet
    val clusterNames = clustering.keySet.toSet

    // assign node IDs
    val stateToID = uniqueStates.zipWithIndex.map{
      case (s, i) => {
        s -> s"N$i"
      }
    }.toMap

    // for each state, find out:
    //   - nb cells
    //   - which clusters have the cell
    val stateToNbCells = uniqueStates.map{ s =>
      s -> stateToMeasurements(s).size
    }.toMap
    val stateToCellsPerCluster = uniqueStates.map{ s =>
      val cellIDs = stateToMeasurements(s).map(_.id)
      val clusters = clusterNames filter { n =>
        clustering(n).intersect(cellIDs.toSet).nonEmpty
      }
      val cellsPerCluster = clusterNames map { n =>
        n -> clustering(n).intersect(cellIDs.toSet).size
      }
      s -> (cellsPerCluster.filter(_._2 > 0).toMap)
    }.toMap

    "digraph G {\n" +
      "graph [layout=\"sfdp\", overlap=\"prism\"];\n" +
      dotNodes(stateToID, stateToNbCells, stateToCellsPerCluster) + "\n" +
      dotEdges(exp, stateToID) + "\n" +
    "}"
  }

  private def dotNodes(
    stateToID: Map[Seq[Int], String],
    stateToNbCells: Map[Seq[Int], Int],
    stateToCellsPerCluster: Map[Seq[Int], Map[String, Int]]
  ): String = {
    val sb = new StringBuilder()
    for ((s, id) <- stateToID) {
      val clustersStr = stateToCellsPerCluster(s).map{
        case (cname, nbCells) => s"$cname (${nbCells})"
      }.mkString("{", ",", "}")
      sb.append(id + " [label=\"" + clustersStr + "\"];\n")
    }
    sb.toString()
  }

  private def dotEdges(
    exp: DiscreteExperiment, stateToID: Map[Seq[Int], String]): String = {

    val sb = new StringBuilder()
    val stateSeq = stateToID.keySet.toIndexedSeq
    for {
      i <- 0 until stateSeq.size
      j <- (i + 1) until stateSeq.size
    } {
      val s1 = stateSeq(i)
      val s2 = stateSeq(j)

      // compute distance and print if it's 1
      val dist = DiscreteStateAnalysis.distance(s1, s2)
      val edgeStyles = Map(
        1 -> "solid",
        2 -> "dashed"
      )
      if (dist <= 1) {
        val style = edgeStyles(dist)

        val diffNames = DiscreteStateAnalysis.nonIdenticalNames(exp, s1, s2)
        assert(diffNames.size == 1)
        val diffName = diffNames.head
        val s1Low = s1(exp.names.indexOf(diffName)) == 0
        val lhs = if (s1Low) s1 else s2
        val rhs = if (s1Low) s2 else s1

        sb append s"${stateToID(lhs)} -> ${stateToID(rhs)} [style=$style," +
          "label=\"" + diffName + "\"];\n"
      }
    }
    sb.toString()
  }

}
