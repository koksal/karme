package karme.external

import org.ddahl.rscala.RClient

abstract class AbstractRInterface[T, U] {

  def process(R: RClient)(arg: T): U

  def run(arg: T): U = {
    val R = RClient()
    val f = process(R) _
    val result = f(arg)
    R.exit()
    result
  }

}
