package karme.external

import org.ddahl.rscala.RClient

abstract class AbstractRInterface {

  def LIBRARIES: Seq[String] = Nil

  val R = RClient()
  importLibraries(R)

  def call(
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
