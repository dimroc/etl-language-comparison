package mapreduce

import akka.actor.ActorSystem
import akka.actor.Props

object Mapper {
  def map(inputDir: String, outputDir: String) = {
    val system = ActorSystem("MapSystem")
    val mapSupervisor = system.actorOf(Props[MapSupervisor], "mapsupervisor")
    val message = ProcessDirectoryMessage(inputDir, outputDir)
    mapSupervisor ! message
  }
}
