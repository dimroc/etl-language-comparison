package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io.File

class MapActor extends Actor {
  def receive = {
    case "hello" => println("hello back at you")
    case _ => println("huh?")
  }
}

object Mapper {
  def map(inputDir: String, outputDir: String) = {
    val inputFiles = generateInputFiles(inputDir)
    val outputFiles = generateOutputFiles(inputFiles, outputDir)
    println(inputFiles mkString "\n")
    println(outputFiles mkString "\n")

    val system = ActorSystem("MapSystem")
    val mapActor = system.actorOf(Props[MapActor], "mapactor")
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
