package karme.util

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.file.FileSystems

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

  def getFileName(path: String): String = {
    FileSystems.getDefault().getPath(path).getFileName().toString()
  }

  def readContent(f: File): String = {
    val source = scala.io.Source.fromFile(f)
    val content = source.getLines().mkString("\n")
    source.close()
    content
  }

  def listFiles(folder: File): Seq[File] = {
    folder.listFiles()
  }

  def getParent(f: File): File = {
    f.getAbsoluteFile.getParentFile
  }

}
