package mapreduce

import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.collection._
import scala.concurrent.Await
import scala.concurrent.duration._

object Mapper {
  implicit val timeout = Timeout(10 minutes)

  def map(inputDir: String): List[immutable.Map[String,Int]] = {
    val system = ActorSystem("MapSystem")

    // Using a supervisor in this example is unnecessary but educational.
    val mapSupervisor = system.actorOf(Props[MapSupervisor], "mapsupervisor")
    val future = mapSupervisor ? ProcessDirectoryMessage(inputDir)

    val results = Await.result(future, Duration.Inf)
    system.shutdown
    results.asInstanceOf[List[immutable.Map[String,Int]]]
  }
}
