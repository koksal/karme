package karme.util

import scala.collection.mutable

object MapUtil {
  def addBinding[A, B](map: Map[A, Set[B]], k: A, v: B): Map[A, Set[B]] = {
    map + (k -> (map.getOrElse(k, Set.empty[B]) + v))
  }

  def removeBinding[A, B](map: Map[A, Set[B]], k: A, v: B): Map[A, Set[B]] = {
    map.get(k) match {
      case None => map
      case Some(vs) if vs == Set(v) => map - k
      case Some(vs) => map + (k -> (vs - v))
    }
  }
}
