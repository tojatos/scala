import akka.actor.{Actor, ActorRef, ActorSystem, Props, ActorLogging}
import scala.util.Random

class SumServer(val name: String, var sum: Int = 0) extends Actor with ActorLogging {
  def receive = {
    case SumServer.Receive(amount) => {
      sum += amount
      log.info(s"${name}: Received amount: ${amount}, sending sum: ${sum}");
      sender ! Sender.CurrentSum(sum)
    }
  }
}

class Sender(val name: String, val amounts: List[Int]) extends Actor with ActorLogging {
  var index = 0
  def sendNext(sender: ActorRef) = {
      if(index < amounts.length)
      {
        log.info(s"${name}: Sending amount: ${amounts(index)}");
        sender ! SumServer.Receive(amounts(index))
        index += 1
      }
  }

  def receive = {
    case Sender.Init() => sendNext(sender)
    case Sender.CurrentSum(amount) => {
      log.info(s"${name}: Received sum: ${amount}");
      sendNext(sender)
    }
  }
}

object SumServer {
  def props(name: String, num: Int = 0) = Props(classOf[SumServer], name, num)
  case class Receive(amount: Int)
}

object Sender {
  def props(name: String, amounts: List[Int]) = Props(classOf[Sender], name, amounts)
  case class CurrentSum(amount: Int)
  case class Init()
}

object Main extends App {
  val ourSystem = ActorSystem()
  val players: Array[Option[ActorRef]] = Array(None, None, None)
  val sumServer: ActorRef = ourSystem.actorOf(SumServer.props("Sum server"))
  val s1: ActorRef = ourSystem.actorOf(Sender.props("s1", List(2, 5, 7)))
  val s2: ActorRef = ourSystem.actorOf(Sender.props("s2", List(3, 1, 3)))
  val s3: ActorRef = ourSystem.actorOf(Sender.props("s2", List(0, 1, 3, 10)))
  val s4: ActorRef = ourSystem.actorOf(Sender.props("s2", List()))
  s1.tell(Sender.Init(), sumServer)
  s2.tell(Sender.Init(), sumServer)
  s3.tell(Sender.Init(), sumServer)
  s4.tell(Sender.Init(), sumServer)

  Thread.sleep(1000)
  ourSystem.terminate
}