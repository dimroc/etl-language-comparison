package mapreduce

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

class MapActor extends Actor {
  def receive = {
    case "hello" => println("hello back at you")
    case _ => println("huh?")
  }
}

object Mapper {
  def map() = {
    val system = ActorSystem("MapSystem")
    val mapActor = system.actorOf(Props[MapActor], "mapactor")
    mapActor ! "hello"
    mapActor ! "buenos dias"
  }
}
