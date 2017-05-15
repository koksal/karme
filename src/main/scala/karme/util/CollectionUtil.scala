package karme.util

object CollectionUtil {

  def orderByCount[T](xs: Seq[T]): Seq[(T, Int)] = {
    val grouped = xs.groupBy(x => x)
    val elemCountPairs = grouped.toSeq map {
      case (x, duplicates) => x -> duplicates.size
    }

    elemCountPairs.sortBy(_._2).reverse
  }

}
