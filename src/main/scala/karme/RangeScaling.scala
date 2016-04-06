package karme

object RangeScaling {
  def scalingFun(
    fromMin: Double, 
    fromMax: Double, 
    toMin: Double, 
    toMax: Double
  ): Double => Double = {
    println(s"Creating scaling f: $fromMin $fromMax -> $toMin $toMax")
    val fromL = fromMax - fromMin
    def f(x: Double): Double = {
      val fromScale = (x - fromMin) / fromL
      toMin * (1 - fromScale) + toMax * fromScale
    }

    f _
  }

  def scalePseudotimes(
    pseudotimes: Array[Double],
    actualTimes: Array[Double]
  ): Array[Double] = {
    val f = scalingFun(
      pseudotimes.min,
      pseudotimes.max,
      actualTimes.min,
      actualTimes.max
    )

    pseudotimes map f
  }
}
