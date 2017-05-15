package karme.util

object CollectionUtil {

  def orderByCount[T](xs: Seq[T]): Seq[(T, Int)] = {
    val grouped = xs.groupBy(x => x)
    val elemCountPairs = grouped.toSeq map {
      case (x, duplicates) => x -> duplicates.size
    }

    elemCountPairs.sortBy(_._2).reverse
  }

  def combineCounts[T](xs: Seq[(T, Int)]): Seq[(T, Int)] = {
    val grouped = xs.groupBy(pair => pair._1)

    val pairsWithCombinedCounts = grouped.toSeq map {
      case (x, duplicates) => {
        val totalCount = duplicates.map(_._2).sum
        x -> totalCount
      }
    }

    pairsWithCombinedCounts.sortBy(_._2).reverse
  }

}
