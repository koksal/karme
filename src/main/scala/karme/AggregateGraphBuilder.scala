package karme

import karme.graphs.StateGraphs.DirectedBooleanStateGraph

object AggregateGraphBuilder {

  val rangeExpanders = List(
    new OptParameterRangeExpander[Option[Double], InputTransformerOpts](
      id = "pseudolog",
      values = List(None, Some(2), Some(5), Some(10)),
      modifier = (opts, value) => opts.copy(pseudoLogFactor = value)
    ),

    new OptParameterRangeExpander[Double, InputTransformerOpts](
      id = "cell-activity",
      values = List(0.1, 0.2, 0.3),
      modifier = (opts, value) => opts.copy(cellActivityThreshold = value)
    ),

    new OptParameterRangeExpander[Double, InputTransformerOpts](
      id = "uncertainty",
      values = List(0.1, 0.2, 0.3, 0.4, 0.5),
      modifier = (opts, value) => opts.copy(uncertaintyThreshold = value)
    ),

    new OptParameterRangeExpander[Int, InputTransformerOpts](
      id = "smoothing-radius",
      values = List(5, 10, 20, 30),
      modifier = (opts, value) => opts.copy(smoothingRadius = value)
    )
  )

  def apply[U](
    baseOpts: InputTransformerOpts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): DirectedBooleanStateGraph = {
    val allOpts = expandOpts(baseOpts, paramRangeExpanders)

    // obtain all clustering results
    // generate graph for each clustering
    // build n-gram model for each graph
    // expand each n-gram model to gene level with its respective clustering
    ???
  }

  def expandOpts(
    baseOpts: InputTransformerOpts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): Seq[InputTransformerOpts] = {
    def step(
      acc: List[InputTransformerOpts],
      pre: OptParameterRangeExpander[_, InputTransformerOpts]
    ): List[InputTransformerOpts] = {
      acc.flatMap(opts => pre.expand(opts))
    }

    paramRangeExpanders.foldLeft(List(baseOpts))(step)
  }
}
