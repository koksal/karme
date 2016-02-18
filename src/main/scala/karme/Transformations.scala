package karme

object Transformations {
  def arcsinh(experiment: Experiment, factor: Double): Experiment = {
    def transFun(v: Double): Double = {
      // arcsinh formula
      val scaled = v / factor
      math.log(scaled + math.sqrt(scaled * scaled + 1))
    }
    val mappedMeasurements = experiment.measurements map { m =>
      val mappedValues = m.values map transFun
      m.copy(values = mappedValues)
    }
    experiment.copy(measurements = mappedMeasurements)
  }

  def normalize(exp: Experiment): Experiment = {
    // for each protein
    //   get all cell values
    //   compute min, max
    // scale all cells by looking up indexed min-max values
    // z = (x - min) / (max - min)
    val minMaxPairs = exp.measuredProteins.toIndexedSeq.zipWithIndex map { 
      case (p, i) =>
        val allValues = exp.measurements map { cm => cm.values(i) }
        (allValues.min, allValues.max)
    }

    val normMeasurements = exp.measurements map { cm =>
      val normValues = cm.values.zipWithIndex map { case (v, i) =>
        val (min, max) = minMaxPairs(i)
        (v - min) / (max - min)
      }
      cm.copy(values = normValues)
    }

    exp.copy(measurements = normMeasurements)
  }

  def allPositive(experiment: Experiment): Experiment = {
    val filteredMs = experiment.measurements filter { m =>
      m.values.forall(_ > 0)
    }
    if (filteredMs.size < experiment.measurements.size)
      println("Filtered out some cells.")
    experiment.copy(measurements = filteredMs)
  }

  def sampleTimePoints(exp: Experiment, seed: Option[Int]): Experiment = {
    val rand = Util.random(seed)
    val cellsPerStep = exp.measurements.groupBy(_.step)
    val minNbCells = cellsPerStep.values.map(_.size).min

    val absoluteMax = 1000
    val nbToSample = math.min(minNbCells, absoluteMax)
    println(s"Sampling ${nbToSample} cells per time step.")

    // inefficient but simple sampling by shuffling
    val sampledCells = cellsPerStep flatMap { case (step, cells) =>
      rand.shuffle(cells).take(nbToSample)
    }

    exp.copy(measurements = sampledCells.toIndexedSeq)
  }

  def filterUntilTime(exp: Experiment, maxTime: Double): Experiment = {
    val fms = exp.measurements filter { m => m.time <= maxTime }
    exp.copy(measurements = fms)
  }

  def shuffleTimeLabels(exp: Experiment, seed: Option[Int]): Experiment = {
    val rand = Util.random(seed)

    val stepTimePairs = exp.measurements map { m => (m.step, m.time) }
    val shuffledStepTimePairs = rand.shuffle(stepTimePairs)

    val shuffledMeasurements = shuffledStepTimePairs.zip(exp.measurements) map {
      case ((step, time), m) => {
        m.copy(step = step, time = time)
      }
    }

    exp.copy(measurements = shuffledMeasurements)
  }
}
