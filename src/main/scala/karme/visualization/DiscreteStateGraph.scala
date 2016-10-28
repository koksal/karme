package karme.visualization

import java.io.File

import karme.Experiments.DiscreteExperiment
import karme.analysis.DiscreteStateAnalysis
import karme.util.FileUtil

import scala.collection.mutable
import scala.sys.process._

object DiscreteStateGraph {

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
    val stateToClusters = uniqueStates.map{ s =>
      val cellIDs = stateToMeasurements(s).map(_.id)
      val clusters = clusterNames filter { n =>
        clustering(n).intersect(cellIDs.toSet).nonEmpty
      }
      s -> clusters
    }.toMap

    "graph G {\n" +
      "graph [layout=\"sfdp\", overlap=\"prism\"];\n" +
      dotNodes(stateToID, stateToNbCells, stateToClusters) + "\n" +
      dotEdges(stateToID) + "\n" +
    "}"
  }

  private def dotNodes(
    stateToID: Map[Seq[Int], String],
    stateToNbCells: Map[Seq[Int], Int],
    stateToClusters: Map[Seq[Int], Set[String]]
  ): String = {
    val sb = new StringBuilder()
    for ((s, id) <- stateToID) {
      val clustersStr = stateToClusters(s).mkString("{", ",", "}")
      sb.append(id + " [label=\"" + clustersStr + "\"];\n")
    }
    sb.toString()
  }

  private def dotEdges(stateToID: Map[Seq[Int], String]): String = {
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
      if (dist <= 2) {
        val style = edgeStyles(dist)
        sb append s"${stateToID(s1)} -- ${stateToID(s2)} [style=$style];\n"
      }
    }
    sb.toString()
  }

}
