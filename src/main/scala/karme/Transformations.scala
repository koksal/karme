package karme

object Transformations {
  private def arcsinh(v: Double, factor: Double): Double = {
    // arcsinh formula
    val scaled = v / factor
    val interior = scaled + math.sqrt(scaled * scaled + 1)
    math.log(interior)
  }

  def arcsinhValues(experiment: Experiment, factor: Double): Experiment = {
    val mappedMeasurements = experiment.measurements map { m =>
      val mappedValues = m.values.map(v => arcsinh(v, factor))
      m.copy(values = mappedValues)
    }
    experiment.copy(measurements = mappedMeasurements)
  }

  def arcsinhTime(experiment: Experiment, factor: Double): Experiment = {
    val mappedMeasurements = experiment.measurements map { m =>
      m.copy(time = arcsinh(m.time, factor))
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

  def normalizeValues(exp: Experiment): Experiment = {
    val minMaxValues = exp.measuredProteins.toIndexedSeq.zipWithIndex map { 
      case (p, i) =>
        val allValues = exp.measurements map { cm => cm.values(i) }
        (allValues.min, allValues.max)
    }

    val normMeasurements = exp.measurements map { cm =>
      val normValues = cm.values.zipWithIndex map { case (v, i) =>
        val (min, max) = minMaxValues(i)
        if (max == min) {
          (v - min)
        } else {
          (v - min) / (max - min)
        }
      }
      cm.copy(values = normValues)
    }

    exp.copy(measurements = normMeasurements)
  }

  def normalizeTime(exp: Experiment): Experiment = {
    val (minTime, maxTime) = {
      val ts = exp.measurements.map(_.time)
      (ts.min, ts.max)
    }

    val normMeasurements = exp.measurements map { cm =>
      val normTime = (cm.time - minTime) / (maxTime - minTime)
      cm.copy(time = normTime)
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

  def allFinite(experiment: Experiment): Experiment = {
    val fms = experiment.measurements.filter{ m =>
      m.values.forall(v => !v.isInfinite)
    }
    experiment.copy(measurements = fms)
  }

  def sampleTimePoints(
    exp: Experiment, 
    seed: Option[Int],
    count: Int
  ): Experiment = {
    val rand = Util.random(seed)
    val cellsPerTime = exp.measurements.groupBy(_.time)
    val minNbCells = cellsPerTime.values.map(_.size).min

    val absoluteMax = count / cellsPerTime.size
    val nbToSample = count // math.min(minNbCells, absoluteMax)
    println(s"Sampling ${nbToSample} cells per time step.")

    // inefficient but simple sampling by shuffling
    // re-sort unsorted map
    val orderedCellGroups = cellsPerTime.toList.sortBy(_._1)
    val sampledCells = orderedCellGroups flatMap { case (time, cells) =>
      println(s"Time step $time: Sampling from ${cells.size} cells.")
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

    val timeValues = exp.measurements map (_.time)
    val shuffledTimeValues = rand.shuffle(timeValues)

    val shuffledMeasurements = shuffledTimeValues.zip(exp.measurements) map {
      case (time, m) => {
        m.copy(time = time)
      }
    }

    exp.copy(measurements = shuffledMeasurements)
  }

  // at each time point, sample cells such that each value range of each protein is represented.
  def sampleValueRange(
    exp: Experiment,
    seed: Option[Int],
    count: Int,
    nbGridCells: Int
  ): Experiment = {
    val rand = Util.random(seed)
    var sampledMeasurements = Set[CellMeasurement]()

    val cellsPerTimeStep = exp.measurements.groupBy(_.time).toList.sortBy(_._1)

    val countPerProtein = count / exp.measuredProteins.size
    val countPerSample = math.max(1, countPerProtein / (cellsPerTimeStep.size * nbGridCells))
    println(s"Count per protein: $countPerProtein")
    println(s"Count per sample: $countPerSample")

    for ((p, i) <- exp.measuredProteins.zipWithIndex) {
      for ((_, stepCells) <- cellsPerTimeStep) {
        val pValues = stepCells.map(_.values(i))
        val (pMin, pMax) = (pValues.min, pValues.max)
        val groupedByValue = stepCells.groupBy { c =>
          val v = c.values(i)
          val normalizedV = (v - pMin) / (pMax - pMin)
          (normalizedV * nbGridCells).toInt
        }
        val selected = groupedByValue flatMap { case (_, groupCells) =>
          rand.shuffle(groupCells).take(countPerSample)
        }
        sampledMeasurements ++= selected
      }
    }

    println(s"Selected a total of ${sampledMeasurements.size} cells.")
    exp.copy(measurements = sampledMeasurements.toIndexedSeq)
  }

  def movingAverage(exp: Experiment, n: Int): Experiment = {
    val msOrderedByPseudotime = exp.measurements.sortBy(_.pseudotime)
    val smoothedMs = msOrderedByPseudotime.zipWithIndex.map{ case (m, i) =>
      // if there are at least n elements so far, take average of last n
      // otherwise, take average of what's before
      val windowStart = math.max(0, i - n + 1)
      val windowEnd = i
      val window = msOrderedByPseudotime.slice(windowStart, windowEnd + 1)
      val smoothedValues = for (j <- 0 until exp.measuredProteins.size) yield {
        val vs = window.map{m => m.values(j) }
        vs.sum / vs.size
      }
      m.copy(values = smoothedValues)
    }
    exp.copy(measurements = smoothedMs)
  }

  def sampleSequence[T](xs: Seq[T]): Seq[T] = {
    val sampleSize = 1000
    val intervalSize = xs.size / sampleSize
    for ((x, i) <- xs.zipWithIndex; if i % intervalSize == 0) yield x
  }

}
