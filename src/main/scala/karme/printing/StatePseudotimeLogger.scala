package karme.printing

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import karme.CellTrajectories.CellTrajectory
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.StateGraphVertex

object StatePseudotimeLogger {
  def savePseudotimes(
    vertices: Iterable[StateGraphVertex],
    trajectories: Iterable[CellTrajectory],
    nodeToID: Map[StateGraphVertex, String],
    outFolder: File
  ): Unit = {
    for ((trajectory, i) <- trajectories.zipWithIndex) {
      val filename = s"state-pseudotimes-${i}.csv"
      val file = new File(outFolder, filename)
      var rows = List[List[String]]()
      for (v <- vertices) {
        StateGraphs.avgNodePseudotime(v, trajectory) match {
          case Some(pt) => {
            rows = List(nodeToID(v), pt.toString) :: rows
          }
          case None =>
        }
      }

      val writer = CSVWriter.open(file)

      val headerRow = List("state id", "average pseudotime")
      writer.writeAll(headerRow +: rows)
    }
  }

}
