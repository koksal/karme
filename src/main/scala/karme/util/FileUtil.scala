package karme.util

import java.io.File

object FileUtil {

  def folder(name: String, parent: Option[File]): File = parent match {
    case Some(p) => new File(p, name)
    case None => new File(name)
  }

}
