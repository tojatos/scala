import scala.annotation.tailrec
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]
object Main {
  def main(args: Array[String]): Unit =
  {
    zad2()
  }
  def zad2() =
  {
    val tt = Node(1,
                   Node(2,
                        Node(4,
                             Empty,
                             Empty
                            ),
                        Empty
                       ),
                   Node(3,
                        Node(5,
                             Empty,
                             Node(6,
                                  Empty,
                                  Empty
                                 )
                            ),
                        Empty
                       )
                  )
    def withOneChild[A](bt: BT[A]): Int = {
      def isEmpty(elem: BT[A]) =
        elem match {
          case Empty => true
          case _ => false
        }
      //def counter(elem: BT[A], acc: Int, ys: List[BT[A]]): (Int, List[BT[A]]) =
      //  elem match {
      //    case Empty => (acc, ys)
      //    case Node(_, l, r) => (acc+(if (isEmpty(l) && !isEmpty(r) || !isEmpty(l) && isEmpty(r)) 1 else 0), r::l::ys)
      //  }

      def iter(xs: List[BT[A]]): Int = xs match {
        case Nil => 0
        case Empty::tl => iter(tl)
        case Node(_, l, r)::tl => (if (isEmpty(l) && !isEmpty(r) || !isEmpty(l) && isEmpty(r)) 1 else 0) + iter(l::r::tl)
      }
      //def iter(accum: Int, xs: List[BT[A]]): Int =
      //  xs match {
      //    case Nil => accum
      //    case xs => xs.foldLeft(0, List[BT[A]]()){case ((acc, queue), elem) => counter(elem, acc, queue)} match {
      //      case (acc, queue) => iter(accum+acc, queue)
      //    }
      //  }
      iter(List(bt))
    }
    println(withOneChild(tt))
  }
}
