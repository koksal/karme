package karme

import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.transformations.InputTransformer

object AggregateGraphBuilder {

  def apply[U](
    baseOpts: Opts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): DirectedBooleanStateGraph = {
    val annotCtx = AnnotationContext.fromOptions(baseOpts.annotationOpts)

    val allOpts = expandOpts(baseOpts.inputTransformerOpts, paramRangeExpanders)

    val clusteringGraphPairs = allOpts flatMap { opt =>
      val transformer = new InputTransformer(opt, annotCtx,
        Reporter.defaultReporter())
      transformer.buildDirectedStateGraphsForAllClusterings()
    }

    // build n-gram model for each graph
    // expand each n-gram model to gene level with its respective clustering
    val bigrams = for ((clustering, graph) <- clusteringGraphPairs) yield {
      expandBigrams(buildBigrams(graph), clustering)
    }

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

  def buildBigrams(graph: DirectedBooleanStateGraph): Seq[(String, String)] = {
    // 1-edge paths (simply edges)
    graph.V.toSeq flatMap { v =>
      graph.targets(v).toSeq map (t => (v, t))
    }

    // flatmap paths to extend each by 1 edge
    ???

    // for each 2-edge path, pair of first and second labels
    ???
  }

  def expandBigrams(
    bigrams: Seq[(String, String)], clustering: Map[String, Set[String]]
  ): Seq[(String, String)] = {
    bigrams flatMap (b => expandBigram(b, clustering))
  }

  def expandBigram(
    bigram: (String, String), clustering: Map[String, Set[String]]
  ): Seq[(String, String)] = {
    for {
      src <- clustering(bigram._1).toSeq
      tgt <- clustering(bigram._2).toSeq
    } yield {
      (src, tgt)
    }
  }
}
