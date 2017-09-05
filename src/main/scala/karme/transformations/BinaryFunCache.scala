package karme.transformations

trait BinaryFunCache[T, U] {

  private var cache: Map[(T, T), U] = Map.empty

  def compute(x: T, y: T): U

  def get(x: T, y: T): U = cache.get((x, y)) match {
    case Some(cached) => cached
    case None => {
      val res = compute(x, y)
      cache = cache.updated((x, y), res)
      res
    }
  }

}
