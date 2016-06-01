package karme

import java.io.File

/** Interpret GNW output as single-cell data. Assumes the data has already been
 *  split into files. */
object GNW2SingleCells {
  def main(args: Array[String]): Unit = {
    val inFnPrefix = args(0)
    val inFnNb = 10000
    val suffixes = (1 to inFnNb).map(i => 
        String.format("%05d", new java.lang.Integer(i))
        )
    val inFs = suffixes.map(suffix => new File(inFnPrefix + suffix))

    val outFn = args(1)
    val outF = new File(outFn)

    val nbSample = 11
    val maxT = 1000.0
    val sts = (0 until nbSample).map(i => i * maxT / (nbSample - 1))
    val exp = readAsSingleCells(inFs, sts)

    FileReporter.outputTuples(outF, exp.toTuples())
  }

  def readAsSingleCells(fs: Seq[File], samplingTimes: Seq[Double]): Experiment = {
    println("Parsing files...")
    val tuplesPerCellFile = fs map Parsers.readGNWTimeSeries
    val firstTuples = tuplesPerCellFile.head
    val names = (firstTuples.head.keySet - "Time").toSeq.sorted

    val rand = Util.random(None)

    println("Selecting cells...")
    val measurements = tuplesPerCellFile map { tuples =>
      // pick a random sampling time
      val samplingT = samplingTimes(rand.nextInt(samplingTimes.size))
      val actualT = samplingT
      val pseudoT = -1.0
      // TODO pick a time delay shift (optimally, following a normal dist.)
      val cellAtSamplingTime = tuples.find(t => math.abs(t("Time").toDouble - samplingT) <= 1).getOrElse {
        throw new Exception("Tried to get cell at time " + samplingT)
      }
      val values = names.map(n => cellAtSamplingTime(n).toDouble).toIndexedSeq
      CellMeasurement(samplingT, actualT, pseudoT, values)
    }
    Experiment(names, measurements.toIndexedSeq)
  }
}
