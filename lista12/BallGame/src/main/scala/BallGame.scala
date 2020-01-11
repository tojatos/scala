import akka.actor.{Actor, ActorRef, ActorSystem, Props, ActorLogging}
import scala.util.Random

class Player(val num: Int, val players: Array[Option[ActorRef]]) extends Actor with ActorLogging {
  def receive = {
    case Player.Ball(count) => {
      log.info(s"Player ID: ${num}, Count number: ${count}");
      players(RandExcept.next(0, players.length, num)).get ! Player.Ball(count+1)
    }
  }
  override def unhandled(msg: Any) =
    msg match {
	  case msg: String => log.info(s"Unexpected message '$msg'")
	  case msg         => super.unhandled(msg)
	}
}

object RandExcept {
  def next(min: Int, max: Int, except: Int): Int = Random.between(min, max) match {
    case a if a == except => next(min, max, except);
    case other => other
  }
}

object Player {
  def props(num: Int, players: Array[Option[ActorRef]]) = Props(classOf[Player], num, players)
  case class Ball(count: Int)
}

object Main extends App {
  val ourSystem = ActorSystem()
  val players: Array[Option[ActorRef]] = Array(None, None, None)
  players(0) = Some(ourSystem.actorOf(Player.props(0, players)))
  players(1) = Some(ourSystem.actorOf(Player.props(1, players)))
  players(2) = Some(ourSystem.actorOf(Player.props(2, players)))
  players(0).get ! Player.Ball(0)

  Thread.sleep(1000)
  ourSystem.terminate
}