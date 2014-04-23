package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io.File

abstract class MapMessage
case class ProcessDirectoryMessage(inputDir: String, outputDir: String) extends MapMessage
case class FileCompletedMessage extends MapMessage

class MapSupervisor extends Actor {
  def receive = {
    case ProcessDirectoryMessage(inputDir, outputDir) => processDirectory(inputDir, outputDir)
    case FileCompletedMessage => println("DONE")
  }

  def processDirectory(inputDir: String, outputDir: String) {
    val inputFiles = generateInputFiles(inputDir)
    val outputFiles = generateOutputFiles(inputFiles, outputDir)
    println(inputFiles mkString "\n")
    println(outputFiles mkString "\n")

    val mapActor = context.actorOf(Props[MapActor], "mapactor")
    mapActor ! "hello"
    mapActor ! "buenos dias"
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
