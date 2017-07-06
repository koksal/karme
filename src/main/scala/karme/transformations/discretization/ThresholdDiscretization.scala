package karme.transformations.discretization

object ThresholdDiscretization {

  val HIGH_VALUE = 2
  val LOW_VALUE = 1

  def apply(vs: Seq[Double]): Seq[Int] = {
    require(vs.forall(_ >= 0))

    vs map { v =>
      if (v > 0) {
        HIGH_VALUE
      } else {
        LOW_VALUE
      }
    }
  }

}
