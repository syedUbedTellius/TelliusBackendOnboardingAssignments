package projects

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorRef, ActorSystem, OneForOneStrategy, Props, SupervisorStrategy}
import projects.com._

import scala.concurrent.duration.DurationInt

case class acc(id:Int,var bal:Int=0)

object com{
  case class addacc(id:Int)
  case class dep(id:Int,am:Int)
  case class wdr(id:Int,am:Int)
  case class tellbal(id:Int)
}

class bankac extends Actor{
  import com._
var accMap:Map[Int,acc]= Map()

  override def receive: Receive = {
    case addacc(id) => {
      accMap.get(id) match{
        case None => accMap=accMap+(id->acc(id))
        case Some(v) => println(s"Acc already exists with id ${id}")
      }
    }
    case dep(id,am)=> {
      accMap.get(id) match {
        case None => println(s"Acc dont exists with id ${id}")
        case Some(v) => v.bal += am
      }
    }
      case wdr(id,am)=> {
        accMap.get(id) match{
          case None => println(s"Acc dont exists with id ${id}")
          case Some(v) => if(v.bal-am>=0) v.bal-=am else println("Cant withdraw more than balance")
        }
    }
    case tellbal(id) =>
      accMap.get(id) match {
        case None => if (context.sender() != context.system.deadLetters) {
          sender() ! s"Acc dont exists with id $id"
          println ("cant withdraw")
        } else println(s"Acc dont exists with id $id")
        case Some(v) => if (context.sender() != context.system.deadLetters) {
          sender() ! v.bal
          println(v.bal)
        } else {
          println(s"${v.bal}")
        }
      }

  }
}


class bankSupervisor extends Actor {
  override val supervisorStrategy: SupervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 3, withinTimeRange = 1.minute) {
      case _: Exception =>
        println("Restarting the child because it got exception")
        Restart
    }

  val bankdb = context.actorOf(Props[bankac], "bankdb")

  override def receive: Receive = {
    case msg => {
      bankdb forward msg
    }
  }
}

object Bank extends App{

  val sys=ActorSystem("mysystem")
  val bankdb=sys.actorOf(Props[bankSupervisor],"bsuper")

  bankdb ! addacc(1)
  bankdb ! tellbal(1)
  bankdb ! dep(1,500)
  bankdb ! tellbal(1)
  bankdb ! wdr(1,200)
  bankdb ! tellbal(1)
  bankdb ! wdr(1,888)
}
