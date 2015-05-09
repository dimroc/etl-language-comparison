package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io._
import scala.collection._
import scala.io._
import scala.util.matching._

case class ProcessFileMessage(input: File)

class MapActor extends Actor {
  val pattern = "(?i)knicks".r

  def receive = {
    case ProcessFileMessage(input) => {
      val hoodCounts = processFile(input)
      sender ! hoodCounts
    }
  }

  def processFile(input: File): immutable.Map[String, Int] = {
    //println(s"mapping $input")

    var map = mutable.Map[String,Int]()
    Source.fromFile(input.getAbsolutePath()).getLines.foreach(line => {
      val tokens = line.split('\t')
      // Substring match
      //val rval = tokens(3).toLowerCase() contains "knicks"
      //if(rval) {
        //map update(tokens(1), 1 + map.getOrElse(tokens(1), 0))
      //}
      val rval = pattern findFirstIn tokens(3)
      rval match {
        case Some(_) => map update(tokens(1), 1 + map.getOrElse(tokens(1), 0))
        case None => ;
      }
    })

    immutable.Map(map.toSeq: _*)
  }
}
