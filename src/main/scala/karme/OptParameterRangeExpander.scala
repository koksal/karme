package karme

class OptParameterRangeExpander[T, U](
  id: String,
  values: Seq[T],
  modifier: (U, T) => U
) {

  def expand(opts: U): Seq[U] = {
    values map { v => modifier(opts, v) }
  }

  override def toString: String = {
    s"Range for $id: ${values.mkString("{", ", ", "}")}"
  }

}

object OptParameterRangeExpander {

  val RANGE_EXPANDERS = List(
    new OptParameterRangeExpander[Option[Double], InputTransformerOpts](
      id = "pseudolog-factor",
      values = List(Some(2)),
      modifier = (opts, value) => opts.copy(pseudoLogFactor = value)
    ),

    new OptParameterRangeExpander[Double, InputTransformerOpts](
      id = "cell-activity-threshold",
      values = List(0, 0.2),
      modifier = (opts, value) => opts.copy(minDifferentialThreshold = value)
    ),

    new OptParameterRangeExpander[Double, InputTransformerOpts](
      id = "uncertainty-threshold",
      values = List(0.5, 1.0),
      modifier = (opts, value) => opts.copy(uncertaintyThreshold = value)
    ),

    new OptParameterRangeExpander[Int, InputTransformerOpts](
      id = "smoothing-radius",
      values = List(0, 10, 20),
      modifier = (opts, value) => opts.copy(smoothingRadius = value)
    )
  )

}
