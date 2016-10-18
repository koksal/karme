package karme.analysis

import karme.DiscreteCellMeasurement
import karme.DiscreteExperiment

import scala.collection.mutable

object DiscreteStateAnalysis {

  def analyze(
    exp: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String]
  ): Unit = {
    // TODO how many unique states are there?
    println("All cells:")
    printUniqueStates(exp.measurements.toSet)

    // TODO how many unique states per cluster?
    val idToMeasurement = exp.measurements.map(m => m.id -> m).toMap
    for ((cluster, ids) <- clustering) {
      println(s"Cluster $cluster")
      val cells = ids map (id => idToMeasurement(id))
      printUniqueStates(cells.toSet)
    }
  }

  private def printUniqueStates(ms: Set[DiscreteCellMeasurement]): Unit = {
    // Discard cell IDs so we collapse unique cell states
    println(s"# All measurements: ${ms.size}")
    println(s"# Unique states: ${nbUniqueStates(ms)}")
  }

  def nbUniqueStates(ms: Set[DiscreteCellMeasurement]): Int = {
    ms.map(_.values).size
  }
}
