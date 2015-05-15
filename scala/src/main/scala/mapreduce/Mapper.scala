package mapreduce

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import mapreduce.MapperUtil._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Mapper {

  /** Map with Akka. */
  def mapWithAkka(inputDir: String): Seq[Map[String, Int]] = {
    implicit val timeout = Timeout(10.minutes)

    val system = ActorSystem("MapSystem")

    // Using a supervisor in this example is unnecessary but educational.
    val mapSupervisor = system.actorOf(Props[MapSupervisor], "mapsupervisor")
    val future = mapSupervisor ? ProcessDirectoryMessage(inputDir)

    val results = Await.result(future, Duration.Inf)
    system.shutdown()
    results.asInstanceOf[List[Map[String, Int]]]
  }

  /** Map with parallel collection. */
  def mapWithPar(inputDir: String): Seq[Map[String, Int]] =
    getInputFiles(inputDir).par.map(processFile).seq

  /** Map with Future. */
  def mapWithFuture(inputDir: String): Seq[Map[String, Int]] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val futures = getInputFiles(inputDir).map { file =>
      Future(processFile(file))
    }
    Await.result(Future.sequence(futures), Duration.Inf)
  }

}
