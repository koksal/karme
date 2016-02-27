package karme

object Transformations {
  private def arcsinh(v: Double, factor: Double): Double = {
    // arcsinh formula
    val scaled = v / factor
    math.log(scaled + math.sqrt(scaled * scaled + 1))
  }

  def arcsinh(experiment: Experiment, factor: Double): Experiment = {
    val mappedMeasurements = experiment.measurements map { m =>
      val mappedValues = m.values.map(v => arcsinh(v, factor))
      m.copy(time = arcsinh(m.time, factor), values = mappedValues)
    }
    experiment.copy(measurements = mappedMeasurements)
  }

  def mean(vs: Seq[Double]): Double = {
    vs.sum / vs.size
  }

  def standardDev(vs: Seq[Double]): Double = {
    val m = mean(vs)
    val N = vs.size - 1
    val variance = vs.map(v => math.pow(v - m, 2)).sum / N
    math.pow(variance, 0.5)
  }

  def scale(vs: Seq[Double]): Seq[Double] = {
    val m = mean(vs)
    val sd = standardDev(vs)
    vs map (v => (v - m) / sd)
  }

  def normalizeProteins(exp: Experiment): Experiment = {
    val minMaxValues = exp.measuredProteins.toIndexedSeq.zipWithIndex map { 
      case (p, i) =>
        val allValues = exp.measurements map { cm => cm.values(i) }
        (allValues.min, allValues.max)
    }

    val (minTime, maxTime) = {
      val ts = exp.measurements.map(_.time)
      (ts.min, ts.max)
    }

    val normMeasurements = exp.measurements map { cm =>
      val normValues = cm.values.zipWithIndex map { case (v, i) =>
        val (min, max) = minMaxValues(i)
        (v - min) / (max - min)
      }
      val normTime = (cm.time - minTime) / (maxTime - minTime)
      cm.copy(time = normTime, values = normValues)
    }

    exp.copy(measurements = normMeasurements)
  }

  def normalizeCells(exp: Experiment): Experiment = {
    val (minTime, maxTime) = {
      val ts = exp.measurements.map(_.time)
      (ts.min, ts.max)
    }

    val normMeasurements = exp.measurements map { cm =>
      val normValues = scale(cm.values).toIndexedSeq
      val normTime = (cm.time - minTime) / (maxTime - minTime)
      cm.copy(time = normTime, values = normValues)
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

  def sampleTimePoints(
    exp: Experiment, 
    seed: Option[Int],
    count: Int
  ): Experiment = {
    val rand = Util.random(seed)
    val cellsPerStep = exp.measurements.groupBy(_.step)
    val minNbCells = cellsPerStep.values.map(_.size).min

    val absoluteMax = count / cellsPerStep.size
    val nbToSample = math.min(minNbCells, absoluteMax)
    println(s"Sampling ${nbToSample} cells per time step.")

    // inefficient but simple sampling by shuffling
    val orderedCellGroups = cellsPerStep.toList.sortBy(_._1)
    val sampledCells = orderedCellGroups flatMap { case (step, cells) =>
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
