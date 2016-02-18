package karme

import com.timeseries.TimeSeries
import com.util.DistanceFunction
import com.util.DistanceFunctionFactory

object DTW {
  def main(args: Array[String]): Unit = {
    val tsI = new TimeSeries(args(0), false, false, ',')
    val tsJ = new TimeSeries(args(1), false, false, ',')

    val distFn = DistanceFunctionFactory.getDistFnByName("EuclideanDistance")
    val radius = 10

    val info = com.dtw.FastDTW.getWarpInfoBetween(tsI, tsJ, radius, distFn)

    println(info.getDistance)
    println(info.getPath)
  }
}
