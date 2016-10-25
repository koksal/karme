package karme.util

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.PrintWriter

object FileUtil {

  def writeToFile(f: File, content: String, append: Boolean = false) {
    // create parent if nonexistent
    val parent = f.getParentFile
    if (parent != null) parent.mkdirs

    val fw = new FileWriter(f.getAbsolutePath, append)
    val bw = new BufferedWriter(fw)
    val out = new PrintWriter(bw)
    try {
      out.print(content)
    } finally {
      out.close
    }
  }

}
