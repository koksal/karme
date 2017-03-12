package karme.external

import org.ddahl.rscala.RClient

abstract class AbstractRInterface[T] {

  val LIBRARIES: Seq[String] = Nil

  def process(R: RClient): T

  def run(): T = {
    val R = RClient()
    importLibraries(R)
    val result = process(R)
    R.exit()
    result
  }

  def call(R: RClient)(
    method: String, resultVar: String, args: (String, _)*
  ): Unit = {
    val argsStr = args.map{
      case (name, value) => s"$name = $value"
    }.mkString(",")

    R.eval(s"$resultVar = $method($argsStr)")
  }

  private def importLibraries(R: RClient): Unit = {
    for (l <- LIBRARIES) {
      R eval s"library($l)"
    }
  }

}
