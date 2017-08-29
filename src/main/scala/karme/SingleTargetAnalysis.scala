package karme

object SingleTargetAnalysis {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOpts(opts.annotationOpts)

    val kdExperiment = InputContext.getKnockdownExpOpt(
      opts.inputFileOpts).getOrElse(sys.error("No KD experiment."))

    for (target <- kdExperiment.targets.toSeq.sorted) {
      runForTarget(target, kdExperiment.sources, opts, annotationContext,
        reporter)
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

    Main.run(opts, reporter, annotationContext, rawExp, trajectories)
  }

}
