package karme

class OptParameterRangeExpander[T, U](
  id: String,
  values: Seq[T],
  modifier: (U, T) => U
) {

  def expand(opts: U): Seq[U] = {
    values map { v => modifier(opts, v) }
  }

}
