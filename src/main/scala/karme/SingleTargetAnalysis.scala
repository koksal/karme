package karme

object SingleTargetAnalysis {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOpts(opts.annotationOpts)

    val kdExperiment = PredictionLibrary.aggregate(
      InputContext.getKnockdownExperiments(opts.inputFileOpts))

    for (target <- kdExperiment.targets.toSeq.sorted) {
      println(s"Running single-target analysis for $target.")
      val ioPairs = runForTarget(target, kdExperiment.sources, opts,
        annotationContext, reporter)

      evaluateForTarget(ioPairs, target, kdExperiment)
    }
  }

  def runForTarget(
    target: String,
    sources: Set[String],
    opts: Opts,
    annotationContext: AnnotationContext,
    reporter: Reporter
  ) = {
    val genesOpt = Some(sources + target)
    val rawExp = InputContext.getRawExperiment(opts.inputFileOpts, genesOpt)
    val trajectories = InputContext.getTrajectories(opts.inputFileOpts)

    val runReporter = reporter.subfolderReporter(s"target-$target")
    Main.runInference(opts, runReporter, annotationContext,
      rawExp, trajectories)

  }

  def evaluateForTarget(
    ioPairs: Seq[(String, String)],
    target: String,
    kdExperiment: PredictionLibrary
  ) = {
    val predictedSourcesForTarget = ioPairs collect {
      case (src, tgt) if tgt == target => src
    }

    val actualSourcesForTarget = kdExperiment.ioPairs collect {
      case (src, tgt) if tgt == target => src
    }

    println(s"Target: $target")
    println(s"Predicted sources: $predictedSourcesForTarget")
    println(s"Actual sources: $actualSourcesForTarget")

    if (predictedSourcesForTarget.nonEmpty) {
      val correctPredictions =
        predictedSourcesForTarget.toSet.intersect(actualSourcesForTarget)
      val hitRatio = correctPredictions.size.toDouble /
        predictedSourcesForTarget.size
      println(s"Hit ratio: $target,$hitRatio")
    }
  }
}
