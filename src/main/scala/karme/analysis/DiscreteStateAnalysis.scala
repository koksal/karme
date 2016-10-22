package karme.analysis

import karme.Experiments.DiscreteExperiment
import karme.Experiments.DiscreteMeasurement
import karme.Experiments.Measurement
import karme.util.MathUtil

import scala.collection.mutable

object DiscreteStateAnalysis {

  def analyze(
    exp: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String]
  ): Unit = {
    println("All cells:")
    printUniqueStates(exp.measurements.toSet)
    printDistances(exp.measurements.toSet)

    for ((cluster, clusterExp) <- exp.partitionClusters(clustering)) {
      println(s"Cluster $cluster")
      printUniqueStates(clusterExp.measurements)
      printDistances(clusterExp.measurements)
    }
  }

  private def printUniqueStates(ms: Iterable[Measurement[Int]]): Unit = {
    println(s"# All measurements: ${ms.size}")
    println(s"# Unique states: ${nbUniqueStates(ms)}")
    val grouped = ms.groupBy(_.values)
    val cardinalitySeq = grouped.toSeq.map(_._2.size).sorted.reverse
    println("Cardinality of discrete states: ")
    println(cardinalitySeq.mkString("\n"))
  }

  def nbUniqueStates(ms: Iterable[DiscreteMeasurement]): Int = {
    ms.map(_.values).toSet.size
  }

  def printDistances(ms: Iterable[DiscreteMeasurement]): Unit = {
    val indexableMs = ms.toIndexedSeq
    val distances = for {
      i <- 0 until ms.size
      j <- (i + 1) until ms.size
    } yield {
      distance(indexableMs(i), indexableMs(j))
    }

    println(s"Median distance: ${MathUtil.median(distances)}")
  }


  def distance(
    m1: DiscreteMeasurement,
    m2: DiscreteMeasurement
  ): Int = {
    val diffs = m1.values.zip(m2.values).map {
      case (v1, v2) => math.abs(v1 - v2)
    }

    diffs.sum
  }
}
