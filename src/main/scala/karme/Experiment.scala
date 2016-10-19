package karme

import scala.collection.mutable

object Experiments {

  case class Experiment[T](
    names: Seq[String],
    measurements: Seq[Measurement[T]]
  ) {
    def valuesForName(name: String): Seq[T] = {
      val i = names.indexOf(name)
      val valuesByName = measurements.map(_.values).transpose
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
  }

  type ContinuousExperiment = Experiment[Double]
  type DiscreteExperiment = Experiment[Int]

  case class Measurement[T](id: String, values: Seq[T])

  type ContinuousMeasurement = Measurement[Double]
  type DiscreteMeasurement = Measurement[Int]
}
