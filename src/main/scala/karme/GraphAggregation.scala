package karme

import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.transformations.InputTransformer

object GraphAggregation {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    apply(opts, OptParameterRangeExpander.RANGE_EXPANDERS)
  }

  def apply[U](
    baseOpts: Opts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): Unit = {
    val annotCtx = AnnotationContext.fromOptions(baseOpts.annotationOpts)

    val allOpts = expandOpts(baseOpts.inputTransformerOpts, paramRangeExpanders)
    println(s"Expanded to ${allOpts.size} options.")

    val clusteringGraphPairs = allOpts.par flatMap { opt =>
      val transformer = new InputTransformer(opt, annotCtx,
        Reporter.defaultReporter())
      transformer.buildDirectedStateGraphsForAllClusterings()
    }
    println("Computed clustering results.")

    val expandedBigrams = clusteringGraphPairs flatMap {
      case (clustering, graph) => {
        expandBigrams(buildBigrams(graph), clustering)
      }
    }
    println("Computed expanded bigrams.")

    val bigramCounts = orderByCount(expandedBigrams.seq)

    for ((bigram, count) <- bigramCounts) {
      println(s"$bigram: $count")
    }

    // TODO merge bigrams into a graph?
    // or evaluate directly against reference libraries
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
    graph.pathNodeSequences(3) map labelPair
  }

  def labelPair(
    nodeSeq: IndexedSeq[StateGraphVertex]
  ): (String, String) = nodeSeq match {
    case IndexedSeq(v1, v2, v3) => {
      val l1 = UndirectedStateGraphOps.edgeLabels(UnlabeledEdge(v1, v2))
      val l2 = UndirectedStateGraphOps.edgeLabels(UnlabeledEdge(v2, v3))

      assert(l1.size == 1 && l2.size == 1, "Multiple labels for edge.")

      (l1.head, l2.head)
    }
    case _ => sys.error(s"Expected 3-node path, received: $nodeSeq")
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

  def orderByCount[T](xs: Seq[T]): Seq[(T, Int)] = {
    val elemCountPairs = xs.toSet map { x: T =>
      x -> xs.count(_ == x)
    }
    elemCountPairs.toSeq.sortBy(_._2).reverse
  }
}
