package mapreduce

import scala.collection._
import scala.collection.mutable.Map
import scalaz._
import Scalaz._
import java.io._
import scala.io._

object Reducer {
  def reduce(mappings: List[immutable.Map[String,Int]], destination: String) = {
    val finalCounts = combine(mappings)
    writeOutput(finalCounts, destination)
  }

  def writeOutput(counts: immutable.Map[String,Int], destination: String) = {
    val file = new File(destination)
    val parent = file.getParentFile();
    if(!parent.exists() && !parent.mkdirs()){
        throw new IllegalStateException("Couldn't create dir: " + parent);
    }

    val writer = new BufferedWriter(new FileWriter(file));
    val sorted = counts.toSeq.sortBy(-_._2) // descending

    for ((hood,count) <- sorted) {
      writer.write(s"${hood}\t$count")
      writer.newLine
    }

    writer.close
  }

  def combine(array: List[immutable.Map[String,Int]]) : immutable.Map[String,Int] =
    array.reduceLeft((a,b) => a |+| b)

  def filesFromInput(inputDir: String) = {
    val file = new File(inputDir)
    if (!file.isDirectory()) {
      throw new Exception("input is not a directory")
    }

    file.listFiles.filter(f => !f.isDirectory())
  }

  def printMaps(arrayOfMaps: List[immutable.Map[String,Int]]) = {
    val mapStrings = for(singleMap <- arrayOfMaps) yield {
      singleMap.map(pair => pair._1+"="+pair._2).mkString("\n")
    }

    mapStrings.foreach(println(_))
  }
}
