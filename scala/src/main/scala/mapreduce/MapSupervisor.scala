package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import java.io.File
import scala.collection._
import scala.concurrent
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

abstract class MapMessage
case class ProcessDirectoryMessage(inputDir: String) extends MapMessage

class MapSupervisor extends Actor {
  def receive = {
    case ProcessDirectoryMessage(inputDir) => sender ! processDirectory(inputDir)
  }

  def processDirectory(inputDir: String): List[immutable.Map[String,Int]] = {
    implicit val timeout = Timeout(5 minutes) // Required for '?' notation
    implicit val system = context.dispatcher

    val inputFiles = generateInputFiles(inputDir)

    val futures = for (file <- inputFiles) yield {
      val basename = file.getName()
      val mapActor = context.actorOf(Props[MapActor], s"mapactor.$basename")
      mapActor ? ProcessFileMessage(file) // Equivalent to mapActor.ask("hello", 5 minutes)
    }

    val collapsedFuture = Future.sequence(futures.toList)
    val result = Await.result(collapsedFuture, 30 minutes).map( x => x.asInstanceOf[immutable.Map[String,Int]] )
    //println(result)
    result
  }

  def generateInputFiles(dir: String) = {
    val file = new File(dir)
    if (!file.isDirectory()) {
      throw new Exception("input is not a directory")
    }

    file.listFiles.filter(f => !f.isDirectory())
  }
}
