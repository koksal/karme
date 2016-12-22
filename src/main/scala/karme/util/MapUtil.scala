package karme.util

import scala.collection.mutable

object MapUtil {
  def emptyMultiMap[A, B]: mutable.MultiMap[A, B] = {
    new mutable.HashMap[A, mutable.Set[B]]() with mutable.MultiMap[A, B]
  }
}
