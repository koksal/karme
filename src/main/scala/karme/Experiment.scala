package karme

import karme.discretization.Discretization
import karme.synthesis.Transitions.ConcreteBooleanState

import scala.collection.mutable

object Experiments {

  case class Experiment[T](
    names: Seq[String],
    measurements: Seq[Measurement[T]]
  ) {

    private lazy val valuesByName = measurements.map(_.values).transpose
    private lazy val idToMeasurement: Map[String, Measurement[T]] =
      measurements.map{ m =>
        m.id -> m
      }.toMap

    def valuesForName(name: String): Seq[T] = {
      val i = names.indexOf(name)
      valuesByName(i)
    }

    def partitionClusters(
      clustering: mutable.MultiMap[String, String]
    ): Map[String, Experiment[T]] = {
      val idToMeasurement = measurements.map(m => m.id -> m).toMap

      val pairs = for ((cluster, ids) <- clustering) yield {
        val sortedIds = ids.toList.sorted
        val clusterMs = sortedIds map (id => idToMeasurement(id))
        cluster -> new Experiment(names, clusterMs)
      }
      pairs.toMap
    }

    def project(ns: Seq[String]): Experiment[T] = {
      val indices = ns map names.indexOf
      val valuesByName = measurements.map(_.values).transpose
      val projectedValuesByName = indices map { i =>
        valuesByName(i)
      }
      val projMs = measurements.zip(projectedValuesByName.transpose) map {
        case (measurement, projectedValues) =>
          measurement.copy(values = projectedValues)
      }
      Experiment(ns, projMs)
    }

    def measurementFromId(id: String): Measurement[T] = idToMeasurement(id)
  }

  def discretizeProbabilisticExperiment(
    exp: ProbabilisticExperiment
  ): DiscreteExperiment = {
    val discreteMs = exp.measurements map { m =>
      val discreteVs = m.values.map { v =>
        assert(v >= 0 && v <= 1)
        if (v >= 0.5) Discretization.HIGH_VALUE else Discretization.LOW_VALUE
      }
      m.copy(values = discreteVs)
    }
    exp.copy(measurements = discreteMs)
  }

  def probabilisticExperimentToThreeValued(
    e: ProbabilisticExperiment
  ): ThreeValuedExperiment = {
    val UNCERTAINTY_MARGIN = 0.05
    val MID_VALUE = 0.5

    val triValuedMs = e.measurements map { m =>
      val triValuedVs = m.values map { v =>
        assert(v >= 0 && v <= 1)
        val tv: ThreeValued = if (math.abs(MID_VALUE - v) <= UNCERTAINTY_MARGIN) {
          Uncertain
        } else if (v < MID_VALUE) {
          Low
        } else {
          High
        }
        tv
      }
      m.copy(values = triValuedVs)
    }
    e.copy(measurements = triValuedMs)
  }

  def booleanStatesToExperiment(
    ss: Set[ConcreteBooleanState]
  ): BooleanExperiment = {
    assert(ss.nonEmpty)
    val names = ss.head.orderedKeys
    val measurements = ss.toSeq.zipWithIndex map {
      case (s, i) =>
        val id = s"m_$i"
        val values = s.orderedValues
        Measurement(id, values)
    }
    Experiment(names, measurements)
  }

  def threeValuedToBooleanSet(v: ThreeValued): Set[Boolean] = v match {
    case Low => Set(false)
    case High => Set(true)
    case Uncertain => Set(false, true)
  }

  sealed trait ThreeValued
  case object Low extends ThreeValued
  case object Uncertain extends ThreeValued
  case object High extends ThreeValued

  type ContinuousExperiment = Experiment[Double]
  type DiscreteExperiment = Experiment[Int]
  type BooleanExperiment = Experiment[Boolean]
  type ProbabilisticExperiment = Experiment[Double]
  type ThreeValuedExperiment = Experiment[ThreeValued]

  case class Measurement[T](id: String, values: Seq[T])

  type ContinuousMeasurement = Measurement[Double]
  type DiscreteMeasurement = Measurement[Int]
  type BooleanMeasurement = Measurement[Boolean]
  type ProbabilisticMeasurement = Measurement[Double]
  type ThreeValuedMeasurement = Measurement[ThreeValued]
}
