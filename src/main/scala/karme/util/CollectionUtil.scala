package karme.util

import scala.util.Random

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

  def jaccardIndex[A](s1: Set[A], s2: Set[A]): Double = {
    val nbCommon = s1.intersect(s2).size
    val nbTotal = s1.union(s2).size
    nbCommon.toDouble / nbTotal
  }

  def nonEmptySubsets[T](xs: Set[T]): Set[Set[T]] = {
    allSubsets(xs) - Set[T]()
  }

  def allSubsets[T](xs: Set[T]): Set[Set[T]] = {
    xs.headOption match {
      case Some(head) => {
        allSubsets(xs - head) flatMap { set =>
          Set(
            set,
            set + head
          )
        }
      }
      case None => {
        Set(Set())
      }
    }
  }

  def randomElement[T](random: Random)(xs: Iterable[T]): T = {
    xs.toVector(random.nextInt(xs.size))
  }

  def randomElements[T](
    random: Random
  )(xs: Iterable[T], size: Int): Iterable[T] = {
    random.shuffle(xs.toList).take(size)
  }
}
