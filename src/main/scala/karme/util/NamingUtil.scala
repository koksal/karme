package karme.util

import karme.Experiments.Experiment
import karme.Experiments.Measurement

object NamingUtil {

  def canonicalizeNames[T](e: Experiment[T]): Experiment[T] = {
    e.copy(measurements = e.measurements.map(canonicalizeNames))
  }

  def canonicalizeNames[T](m: Measurement[T]): Measurement[T] = {
    m.copy(state = m.state.mapKeys(canonicalize))
  }

  def canonicalize(name: String): String = {
    name.toUpperCase()
  }

  def selectNames(
    namesToPrune: Set[String], filterNames: Set[String]
  ): Set[String] = {
    val canonicalFilterNames = filterNames map NamingUtil.canonicalize
    namesToPrune filter { n =>
      canonicalFilterNames contains NamingUtil.canonicalize(n)
    }
  }

}
