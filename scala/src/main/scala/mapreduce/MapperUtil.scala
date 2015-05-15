package mapreduce

import java.io.File

import scala.collection.mutable
import scala.io.Source

object MapperUtil {

  def getInputFiles(dir: String): Seq[File] = {
    val file = new File(dir)
    if (!file.isDirectory) {
      throw new Exception("input is not a directory")
    }
    file.listFiles.filter(f => !f.isDirectory)
  }

  val pattern = "(?i)knicks".r

  def processFile(input: File): Map[String, Int] = {
    //println(s"mapping $input")

    val mutableMap = mutable.Map.empty[String, Int]
    val lines = Source.fromFile(input.getAbsolutePath).getLines()
    lines.foreach { line =>
      val tokens = line.split('\t')

      // Substring match
      //val rval = tokens(3).toLowerCase() contains "knicks"
      //if(rval) {
      //map update(tokens(1), 1 + map.getOrElse(tokens(1), 0))
      //}
      val rval = pattern findFirstIn tokens(3)
      val key = tokens(1)
      rval match {
        case Some(_) => mutableMap.update(key, 1 + mutableMap.getOrElse(key, 0))
        case None =>
      }
    }
    mutableMap.toMap
  }
}
