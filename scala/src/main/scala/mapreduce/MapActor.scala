package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io._
import scala.io._
import scala.util.matching._

case class ProcessFileMessage(input: File, output: File)

class MapActor extends Actor {
  val pattern = "(?i)knicks".r

  def receive = {
    case ProcessFileMessage(input, output) => {
      processFile(input, output)
      sender ! "DONE"
    }
  }

  def mapStrategy(hood_id: String, hood: String, borough: String, message: String) = {
    val rval = pattern findFirstIn message
    rval match {
      case Some(_) => s"${hood}\t1"
      case None => s"${hood}\t0"
    }
  }

  def processFile(input: File, output: File) = {
    println(s"mapping $input to $output")

    val writer = new BufferedWriter(new FileWriter(output));
    Source.fromFile(input.getAbsolutePath()).getLines.foreach(line => {
      val tokens = line.split('\t')
      writer write(mapStrategy(tokens(0), tokens(1), tokens(2), tokens(3)))
      writer newLine
    })

    writer close
  }
}
