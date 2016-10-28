package karme

/**
  * Created by ask on 10/27/16.
  */
object CellTrajectories {

  type CellTrajectory = Map[String, Double]

  def cellOrder(trajectory: CellTrajectory): Seq[String] = {
    trajectory.toSeq.sortBy(_._2).map(_._1)
  }

}
