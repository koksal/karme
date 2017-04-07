package karme

class OptParameterRangeExpander[T](
  id: String,
  values: Seq[T],
  modifier: (Opts, T) => Opts
) {

  def expand(opts: Opts): Seq[Opts] = {
    values map { v => modifier(opts, v) }
  }

}
