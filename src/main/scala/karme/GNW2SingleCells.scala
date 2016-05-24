package karme

import java.io.File

/** Interpret GNW output as single-cell data */
object GNW2SingleCells {
  def main(args: Array[String]): Unit = {
    val inFn = args(0)
    val inF = new File(inFn)
    val outFn = args(1)
    val outF = new File(outFn)
    val nbSample = 3
    val maxT = 1000.0
    val sts = (0 until nbSample).map(i => i * maxT / (nbSample - 1))
    val exp = readAsSingleCells(inF, sts)

    FileReporter.outputTuples(outF, exp.toTuples())
  }

  def readAsSingleCells(f: File, samplingTimes: Seq[Double]): Experiment = {
    val tuples = Parsers.readGNWTimeSeries(f)
    val names = (tuples.head.keySet - "Time").toSeq.sorted

    def assignSamplingTime(actualT: Double): Double = {
      samplingTimes.minBy(t => math.abs(t - actualT))
    }

    def createCellMeasurement(tuple: Map[String, String]): CellMeasurement = {
      val actualT = tuple("Time").toDouble
      val samplingT = assignSamplingTime(actualT)
      val pseudoT = -1.0
      val values = names.map(n => tuple(n).toDouble).toIndexedSeq
      CellMeasurement(samplingT, actualT, pseudoT, values)
    }

    val cms = tuples map createCellMeasurement
    Experiment(names, cms.toIndexedSeq)
  }
}
