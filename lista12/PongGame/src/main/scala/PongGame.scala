import akka.actor.{Actor, ActorRef, ActorSystem, Props, ActorLogging}

class PongGame(name: String, var timesLeft: Int) extends Actor with ActorLogging {
  def receive = {
    case PongGame.Ping => {
      if (timesLeft != 0) {
        log.info(s"${name}: ping")
        sender ! PongGame.Pong
        timesLeft -= 1
      }
    }
    case PongGame.Pong => {
      if (timesLeft != 0) {
        log.info(s"${name}: pong")
        sender ! PongGame.Ping
        timesLeft -= 1
      }
    }
  }
  override def unhandled(msg: Any) =
    msg match {
	  case msg: String => log.info(s"Unexpected message '$msg'")
	  case msg         => super.unhandled(msg)
	}  
}

object PongGame {
  def props(name: String, timesLeft: Int) = Props(classOf[PongGame], name, timesLeft)
  case class Ping()
  case class Pong()
}

object Main extends App {
  val ourSystem = ActorSystem()
  val player1: ActorRef = ourSystem.actorOf(PongGame.props("Andrzej", 3)) 
  val player2: ActorRef = ourSystem.actorOf(PongGame.props("Ania", 1)) 
  player1.tell(PongGame.Ping, player2)

  Thread.sleep(1000)
  ourSystem.terminate
}