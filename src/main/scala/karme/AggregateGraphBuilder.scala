package karme

import karme.graphs.StateGraphs.DirectedBooleanStateGraph

object AggregateGraphBuilder {

  // create range expanders for:
  // - pseudolog factor?
  // - cell activity threshold
  // - uncertainty threshold
  // - smoothing radius
  // - # clusters


  def apply[U](
    baseOpts: InputTransformerOpts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): DirectedBooleanStateGraph = {
    val allOpts = expandOpts(baseOpts, paramRangeExpanders)

    // obtain collection of graphs for different nb clusters
    // flatmap graphs
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
