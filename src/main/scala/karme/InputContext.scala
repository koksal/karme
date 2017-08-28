package karme

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.evaluation.enrichr.EnrichrPredictionLibraryParser
import karme.parsing.{CellTrajectoryParser, ContinuousExperimentParser, NamesParser}

case class InputContext(
  rawExperiment: Experiment[Double],
  trajectories: Seq[CellTrajectory],
  knockdownExperiment: Option[PredictionLibrary]
)

object InputContext {

  def fromOpts(opts: InputFileOpts): InputContext = {
    val trajectories = opts.trajectoryFiles map CellTrajectoryParser.parse

    val genes = NamesParser.parseNameUnion(opts.namesFiles)

    val rawExperiment = opts.continuousExperimentFile match {
      case Some(f) => ContinuousExperimentParser.parseAndFilter(f, genes)
      case None => sys.error("No raw experiment file given.")
    }

    val knockdownExp = opts.knockdownExperimentFile map {
      f => EnrichrPredictionLibraryParser(f)
    }

    InputContext(rawExperiment, trajectories, knockdownExp)
  }

}
