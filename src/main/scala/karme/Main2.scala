package karme

object Main2 {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOpts(opts.annotationOpts)
    val inputContext = InputContext.fromOpts(opts.inputFileOpts)

  }

  def run(
    opts: Opts,
    reporter: Reporter,
    annotationContext: AnnotationContext,
    inputContext: InputContext
  ): Unit = {
    // Inferencer module that takes parsed stuff, produces predictions
    // (functions)

    // Evaluation module
  }

}
