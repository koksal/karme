package karme.external

import org.ddahl.rscala.RClient

abstract class AbstractRInterface[T] {

  val libraries: Seq[String]

  def process(R: RClient): T

  def run(): T = {
    val R = RClient()
    importLibraries(R)
    val result = process(R)
    R.exit()
    result
  }

  private def importLibraries(R: RClient): Unit = {
    for (l <- libraries) {
      R eval s"library($l)"
    }
  }

}
