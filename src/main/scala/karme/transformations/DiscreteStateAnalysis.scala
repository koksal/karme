package karme.transformations

import karme.Experiments.DiscreteExperiment
import karme.Experiments.DiscreteMeasurement
import karme.Experiments.Measurement
import karme.synthesis.Transitions.ConcreteBooleanState
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
    distance(m1.values, m2.values)
  }

  def distance(vs1: Seq[Int], vs2: Seq[Int]): Int = {
    val diffs = vs1.zip(vs2) map {
      case (v1, v2) => math.abs(v1 - v2)
    }
    diffs.sum
  }

  def hammingDistance(
    s1: ConcreteBooleanState, s2: ConcreteBooleanState
  ): Int = {
    assert(s1.orderedKeys == s2.orderedKeys)
    s1.orderedValues.zip(s2.orderedValues).count{
      case (v1, v2) => v1 != v2
    }
  }

  def nonIdenticalNames(
    exp: DiscreteExperiment,
    vs1: Seq[Boolean],
    vs2: Seq[Boolean]
  ): Seq[String] = {
    nonIdenticalNames(exp.names, vs1, vs2)
  }

  def nonIdenticalNames(
    names: Seq[String],
    vs1: Seq[Boolean],
    vs2: Seq[Boolean]
  ): Seq[String] = {
    nonIdenticalIndices(vs1, vs2) map { i => names(i) }
  }

  def nonIdenticalIndices[T](vs1: Seq[T], vs2: Seq[T]): Seq[Int] = {
    assert(vs1.size == vs2.size)
    vs1.indices filter { i =>
      vs1(i) != vs2(i)
    }
  }
}
