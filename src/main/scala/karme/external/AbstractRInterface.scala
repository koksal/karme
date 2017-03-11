package karme.external

import karme.visualization.BoxPlot
import org.ddahl.rscala.RClient

abstract class AbstractRInterface[T, U] {

  val libraries: Seq[String]

  def process(R: RClient)(arg: T): U

  def run(arg: T): U = {
    val R = RClient()
    importLibraries(R)
    val f = process(R) _
    val result = f(arg)
    R.exit()
    result
  }

  private def importLibraries(R: RClient): Unit = {
    for (l <- libraries) {
      R eval s"library($l)"
    }
  }

}
