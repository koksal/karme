package karme.discretization

import karme.Experiment

case class DiscretizationResult(nbLevels: Int, values: Seq[Int])

object Discretization {
  // send each protein to ckmeans
  // read back number of clusters and discretized data
  def discretizeExperiment(e: Experiment) = {
  
  }

  def discretizeProtein(xs: Seq[Double]): DiscretizationResult = {
    karme.RInterface.ckmeans(xs)
  }
}
