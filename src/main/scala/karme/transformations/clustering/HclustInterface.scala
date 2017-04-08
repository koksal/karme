package karme.transformations.clustering

import karme.Experiments.Experiment
import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class HclustInterface(
  exp: Experiment[Double], kMax: Int
) extends AbstractRInterface[Seq[Map[String, Int]]] {

  def process(R: RClient): Seq[Map[String, Int]] = {
    require(kMax >= 1)

    val valuesPerVariable = exp.valueMatrix.map(_.toArray).toArray

    R.set("valuesPerVariable", valuesPerVariable)
    R.eval("varClustering = hclust(dist(valuesPerVariable))")
    R.eval(s"varClusterAssignments = cutree(varClustering, k = 1:${kMax})")

    // a matrix where each column corresponds to one element of vector k
    val varClusterAssignments = R.getI2("varClusterAssignments")
    val assignmentsPerK = varClusterAssignments.transpose
    assert(assignmentsPerK.size == kMax)

    assignmentsPerK map { assignment =>
      assert(assignment.size == exp.names.size)
      exp.names.zip(assignment).toMap
    }
  }

}
