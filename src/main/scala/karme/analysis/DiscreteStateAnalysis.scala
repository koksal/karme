package karme.analysis

import karme.DiscreteCellMeasurement
import karme.DiscreteExperiment
import karme.util.MathUtil

import scala.collection.mutable

object DiscreteStateAnalysis {

  def analyze(
    exp: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String]
  ): Unit = {
    // TODO how many unique states are there?
    println("All cells:")
    printUniqueStates(exp.measurements.toSet)
    printDistances(exp.measurements.toSet)

    // TODO how many unique states per cluster?
    val idToMeasurement = exp.measurements.map(m => m.id -> m).toMap
    for ((cluster, ids) <- clustering) {
      println(s"Cluster $cluster")
      val cells = ids map (id => idToMeasurement(id))
      printUniqueStates(cells.toSet)
      printDistances(cells.toSet)
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

  def printDistances(ms: Set[DiscreteCellMeasurement]): Unit = {
    val distances = for {
      m1 <- ms
      m2 <- ms
      if m1 != m2
    } yield {
      distance(m1, m2)
    }

    println(s"Median distance: ${MathUtil.median(distances)}")
  }


  def distance(
    m1: DiscreteCellMeasurement,
    m2: DiscreteCellMeasurement
  ): Int = {
    val diffs = m1.values.zip(m2.values).map {
      case (v1, v2) => math.abs(v1 - v2)
    }

    diffs.sum
  }
}
