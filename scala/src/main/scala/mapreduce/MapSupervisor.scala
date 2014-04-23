package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io.File
import scala.concurrent
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout

abstract class MapMessage
case class ProcessDirectoryMessage(inputDir: String, outputDir: String) extends MapMessage

class MapSupervisor extends Actor {
  def receive = {
    case ProcessDirectoryMessage(inputDir, outputDir) => sender ! processDirectory(inputDir, outputDir)
  }

  def processDirectory(inputDir: String, outputDir: String) {
    implicit val timeout = Timeout(5 minutes) // Required for '?' notation
    implicit val system = context.dispatcher

    val inputFiles = generateInputFiles(inputDir)
    val outputFiles = generateOutputFiles(inputFiles, outputDir)
    println(inputFiles mkString "\n")
    println(outputFiles mkString "\n")

    val futures = for (fileTuple <- inputFiles.zip(outputFiles)) yield {
      val basename = fileTuple._1.getName()
      val mapActor = context.actorOf(Props[MapActor], s"mapactor.$basename")
      mapActor ? "hello" // Equivalent to mapActor.ask("hello", 5 minutes)
    }

    val collapsedFuture = Future.sequence(futures.toList)
    println(Await.result(collapsedFuture, 30 minutes))
  }

  def generateInputFiles(dir: String) = {
    val file = new File(dir)
    if (!file.isDirectory()) {
      throw new Exception("input is not a directory")
    }
    file.listFiles
  }

  def generateOutputFiles(inputFiles: Array[File], dir: String) = {
    val outputDirectory = new File(dir)
    outputDirectory.mkdirs()

    for ((value,index) <- inputFiles.zipWithIndex)
      yield s"${dir}output_$index"
  }
}
