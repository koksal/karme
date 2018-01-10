package karme.printing

object LatexPrinting {

  def latexMath(s: String): String = {
    "$" + s + "$"
  }

  def latexifyId(id: String): String = {
    id.replaceAllLiterally("_", "\\_")
  }

  def latexifyBool(bool: Boolean): String = {
    s"\\mathit{$bool}"
  }

}
