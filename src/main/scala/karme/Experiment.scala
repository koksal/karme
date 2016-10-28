package karme

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

  type ContinuousExperiment = Experiment[Double]
  type DiscreteExperiment = Experiment[Int]
  type ProbabilisticExperiment = Experiment[Double]

  case class Measurement[T](id: String, values: Seq[T])

  type ContinuousMeasurement = Measurement[Double]
  type DiscreteMeasurement = Measurement[Int]
  type ProbabilisticMeasurement = Measurement[Double]
}
