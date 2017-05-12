package karme

import karme.parsing.ClusteringParser
import karme.parsing.NamesParser

case class AnnotationContext(
  annotationVariables: Set[String],
  cellClustering: Map[String, Set[String]]
)

object AnnotationContext {
  def fromOptions(annotationOpts: AnnotationOpts): AnnotationContext = {
    val annotVars = new NamesParser(
      annotationOpts.annotationsFiles).names.getOrElse(Set.empty)

    AnnotationContext(
      annotationVariables = annotVars,
      cellClustering = getClustering(annotationOpts)
    )
  }

  def getClustering(
    annotationOpts: AnnotationOpts
  ): Map[String, Set[String]] = {
    annotationOpts.cellClusteringFile match {
      case Some(f) => ClusteringParser(f)
      case None => Map.empty
    }
  }
}
