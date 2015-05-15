package mapreduce

import java.io._

import akka.actor.Actor
import mapreduce.MapperUtil._

case class ProcessFileMessage(input: File)

class MapActor extends Actor {
  val pattern = "(?i)knicks".r

  def receive = {
    case ProcessFileMessage(input) => {
      val hoodCounts = processFile(input)
      sender ! hoodCounts
    }
  }

}
