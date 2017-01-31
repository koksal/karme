package karme.clustering

import karme.Experiments.Experiment
import org.ddahl.rscala.RClient

object HclustInterface {

  def clusterAndCutree(exp: Experiment[Double], k: Int): Map[String, Int] = {
    val R = RClient()

    // transform experiment to 2D array
    val valuesPerVariable = exp.names.toArray map { n =>
      exp.valuesForName(n).toArray
    }

    R.set("valuesPerVariable", valuesPerVariable)
    R.eval("clustering <- hclust(dist(valuesPerVariable))")
    R.eval(s"clusterAssignments <- cutree(clustering, k = ${k})")

    val clusterAssignments = R.getI1("clusterAssignments")
    assert(exp.names.size == clusterAssignments.size)

    exp.names.zip(clusterAssignments).toMap
  }

}
