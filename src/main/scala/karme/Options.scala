package karme

import java.io.File

case class Options(
  continuousExperimentFile: File = null,
  discreteExperimentFile: File = null,
  clusterFile: File = null
)
