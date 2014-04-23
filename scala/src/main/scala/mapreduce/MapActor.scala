package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io.File

class MapActor extends Actor {
  def receive = {
    case "hello" => {
      println("hello back at you")
      sender ! "DONE"
    }
  }
}
