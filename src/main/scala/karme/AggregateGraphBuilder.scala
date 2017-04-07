package karme

import karme.graphs.StateGraphs.DirectedBooleanStateGraph

object AggregateGraphBuilder {

  def apply(
    baseOpts: Opts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_]]
  ): DirectedBooleanStateGraph = {
    val allOpts = expandOpts(baseOpts, paramRangeExpanders)

    // obtain collection of graphs for different nb clusters
    // flatmap graphs
    ???
  }

  def expandOpts(
    baseOpts: Opts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_]]
  ): Seq[Opts] = {
    def step(acc: List[Opts], pre: OptParameterRangeExpander[_]): List[Opts] = {
      acc.flatMap(opts => pre.expand(opts))
    }
    paramRangeExpanders.foldLeft(List(baseOpts))(step)
  }
}
