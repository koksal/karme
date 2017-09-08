package karme

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.evaluation.enrichr.PredictionLibraryParser
import karme.parsing.{CellTrajectoryParser, ContinuousExperimentParser, NamesParser}

case class InputContext(
  rawExperiment: Experiment[Double],
  trajectories: Seq[CellTrajectory],
  knockdownExperiments: Seq[PredictionLibrary]
)

object InputContext {

  def fromOpts(opts: InputFileOpts): InputContext = {
    val genes = NamesParser.parseNameUnion(opts.namesFiles)

    InputContext(
      getRawExperiment(opts, genes),
      getTrajectories(opts),
      getKnockdownExperiments(opts)
    )
  }

  def getRawExperiment(
    opts: InputFileOpts, geneNames: Option[Set[String]]
  ): Experiment[Double] = {
    opts.continuousExperimentFile match {
      case Some(f) => ContinuousExperimentParser.parseAndFilter(f, geneNames)
      case None => sys.error("No raw experiment file given.")
    }
  }

  def getTrajectories(
    opts: InputFileOpts
  ): Seq[CellTrajectory] = {
    opts.trajectoryFiles map CellTrajectoryParser.parse
  }

  def getKnockdownExperiments(
    opts: InputFileOpts
  ): Seq[PredictionLibrary] = {
    opts.knockdownExperimentFiles map {
      f => PredictionLibraryParser(f)
    }
  }

}
