package karme.util

object NamingUtil {

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
