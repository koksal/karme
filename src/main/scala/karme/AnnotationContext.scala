package karme

import karme.parsing.{ClusteringParser, NamesParser}

case class AnnotationSet(id: String, names: Set[String])

case class AnnotationContext(
  annotationSets: Seq[AnnotationSet],
  cellClustering: Map[String, Set[String]]
)

object AnnotationContext {
  def fromOpts(annotationOpts: AnnotationOpts): AnnotationContext = {
    val annotSets = annotationOpts.annotationsFiles map { f =>
      val names = NamesParser.parseNames(f)
      AnnotationSet(f.getName, names)
    }

    AnnotationContext(
      annotationSets = annotSets,
      cellClustering = getCellClustering(annotationOpts)
    )
  }

  def getCellClustering(
    annotationOpts: AnnotationOpts
  ): Map[String, Set[String]] = {
    annotationOpts.cellClusteringFile match {
      case Some(f) => ClusteringParser(f)
      case None => Map.empty
    }
  }
}
