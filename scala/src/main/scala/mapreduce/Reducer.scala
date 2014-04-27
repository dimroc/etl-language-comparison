package mapreduce

import scala.collection._
import scala.collection.mutable.Map
import scalaz._
import Scalaz._
import java.io._
import scala.io._

object Reducer {
  def reduce(inputDir: String, destination: String) = {
    val files = filesFromInput(inputDir)

    // Reduce files sequentially to be consistent with other language implementations.
    // Which is a shame since it's so easy to go parallel.
    // At this scale, there's no need to read the maps from files, but let's make it mimic something of scale.
    val maps = for (file <- files)
      yield retrieveHoodCounts(file)

    val finalCounts = combine(maps)
    writeOutput(finalCounts, destination)
  }

  def retrieveHoodCounts(file: File): immutable.Map[String, Int] = {
    println(s"reducing ${file.getName()}...")

    def retrieveHoodCountsFromLine(line: String) : (String, Int) = {
      val tokens = line.split('\t') // hood\tcount
      (tokens(0), tokens(1).toInt)
    }

    var map = mutable.Map[String,Int]()
    io.Source.fromFile(file.getAbsolutePath()).getLines().foreach(line => {
      val rval = retrieveHoodCountsFromLine(line)
      map update(rval._1, rval._2 + map.getOrElse(rval._1, 0))
    })

    immutable.Map(map.toSeq: _*)
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

  def combine(array: Array[immutable.Map[String,Int]]) : immutable.Map[String,Int] =
    array.reduceLeft((a,b) => a |+| b)

  def filesFromInput(inputDir: String) = {
    val file = new File(inputDir)
    if (!file.isDirectory()) {
      throw new Exception("input is not a directory")
    }

    file.listFiles.filter(f => !f.isDirectory())
  }

  def printMaps(arrayOfMaps: Array[immutable.Map[String,Int]]) = {
    val mapStrings = for(singleMap <- arrayOfMaps) yield {
      singleMap.map(pair => pair._1+"="+pair._2).mkString("\n")
    }

    mapStrings.foreach(println(_))
  }
}
