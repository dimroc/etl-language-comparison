package mapreduce

import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.util.Timeout

object Mapper {
  implicit val timeout = Timeout(10 minutes)

  def map(inputDir: String, outputDir: String) = {
    val system = ActorSystem("MapSystem")

    // Using a supervisor in this example is unnecessary but educational.
    val mapSupervisor = system.actorOf(Props[MapSupervisor], "mapsupervisor")
    val future = mapSupervisor ? ProcessDirectoryMessage(inputDir, outputDir)

    Await.result(future, Duration.Inf)
    system.shutdown
  }
}
