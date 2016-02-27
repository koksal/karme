package karme

object ArgHandling {
  def parseOptions(args: Array[String]) = {
    val opts = new Options()
    ArgHandling.parser.parse(args, opts) getOrElse {
      sys.error("Bad arguments.")
    }
  }

  private def parser = {
    new scopt.OptionParser[Options]("karme") {
      head("karme", "1.0")

      opt[String]("proteins") required() action { (v, o) =>
        o.copy(proteinNamesPath = new java.io.File(v))
      } text("input protein name file")

      opt[String]("experiment") required() action { (v, o) =>
        o.copy(experimentPath = new java.io.File(v))
      } text("input experiment file in CSV format")

      opt[String]("outlabel") action { (v, o) =>
        o.copy(outLabel = Some(v)) 
      } text("prefix that will be added to output file names")

      opt[String]("outfolder") action { (v, o) =>
        o.copy(outFolder = new java.io.File(v)) 
      } text("folder that output files should be created in")

      opt[Unit]("positive")  action { (_, o) =>
        o.copy(filterPositive = true)
      } text("filter data to cells with all positive values")

      opt[Int]("sample")  action { (v, o) =>
        o.copy(sampleCount = Some(v))
      } text("sample each time point for the same # cells for the given total")

      opt[Int]("seed") action { (v, o) =>
        o.copy(seed = Some(v))
      } text("seed value for random sampling")

      opt[Double]("timelimit") action { (v, o) =>
        o.copy(maxTime = Some(v))
      } text("maximum time value to filter data")

      opt[Double]("arcsinh") action { (v, o) =>
        o.copy(arcsinhFactor = v)
      } text("arcsinh transformation scaling factor")

      opt[Double]("alpha") action { (v, o) =>
        o.copy(propagationAlpha = v)
      } text("alpha value for label propagation")

      opt[Int]("neighbors") action { (v, o) =>
        o.copy(propagationNbNeighbors = v)
      } text("# neighbors for label propagation")

      opt[Double]("timeweight") action { (v, o) =>
        o.copy(propagationTimeWeight = v)
      } text("relative weight of time difference in label propagation")

      opt[Int]("iterations") action { (v, o) =>
        o.copy(propagationNbIter = v)
      } text("# iterations for label propagation")

      opt[Double]("split") action { (v, o) =>
        o.copy(propagationSplitTime = Some(v))
      } text("time value to split propagation analysis")

      help("help") text("print this help message")
    }
  }
}
