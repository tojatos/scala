import scala.annotation.tailrec

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

sealed trait Graphs[A]
case class Graph[A](succ: A=>List[A]) extends Graphs[A] 

object Main {
  def main(args: Array[String]): Unit =
  {
    zad5()
  }
  def zad3() =
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
    def breadthBT[A](bt: BT[A]): List[A] = {
      def iter(xs: List[BT[A]]): List[A] = xs match {
        case Nil => Nil
        case Empty::tl => iter(tl)
        case Node(v, l, r)::tl => v::iter(tl ::: List(l,r))
      }
      iter(List(bt))
    }
    println(breadthBT(tt))
  }
  def zad4() =
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
    def inPath[A](bt: BT[A]): Int = {
      def counter(elem: BT[A], n: Int, acc: Int, ys: List[BT[A]]): (Int, List[BT[A]]) =
        elem match {
          case Empty => (acc, ys)
          case Node(_, l, r) => (acc+n, r::l::ys)
        }

      def iter(accum: Int, n: Int, xs: List[BT[A]]): Int =
        xs match {
          case Nil => accum
          case xs => xs.foldLeft(0, List[BT[A]]()){case ((acc, queue), elem) => counter(elem, n, acc, queue)} match {
            case (acc, queue) => iter(accum+acc, n+1, queue)
          }
        }
      iter(0,0,List(bt))
    }
    def outPath[A](bt: BT[A]): Int = {
      def counter(elem: BT[A], n: Int, acc: Int, ys: List[BT[A]]): (Int, List[BT[A]]) =
        elem match {
          case Empty => (acc+n, ys)
          case Node(_, l, r) => (acc, r::l::ys)
        }

      def iter(accum: Int, n: Int, xs: List[BT[A]]): Int =
        xs match {
          case Nil => accum
          case xs => xs.foldLeft(0, List[BT[A]]()){case ((acc, queue), elem) => counter(elem, n, acc, queue)} match {
            case (acc, queue) => iter(accum+acc, n+1, queue)
          }
        }
      iter(0,0,List(bt))
    }
    println(inPath(tt))
    println(outPath(tt))
  }
  def zad5() =
  {
    val g = Graph((i: Int) => i match {
      case 0 => List(3)
      case 1 => List(0,2,4)
      case 2 => List(1)
      case 3 => Nil
      case 4 => List(0,2)
      case n => throw new Exception("Graph g: node " + n
        + " doesn't exist")
    })

    //def breadthSearch[A] (g: Graph[A]) (startNode: A): List[A] = {
    //  def search(visited: List[A])(queue: List[A]): List[A] = queue match {
    //    case Nil => Nil
    //    case h::t =>
    //      if (visited contains h) search(visited)(t)
    //      else h::search(h::visited)(t ++ (g succ h))
    //  }
    //  search (Nil) (List(startNode))
    //}
    def depthSearch[A] (g: Graph[A]) (startNode: A): List[A] = {
      def search(visited: List[A])(queue: List[A]): List[A] = queue match {
        case Nil => Nil
        case h::t =>
          if (visited contains h) search(visited)(t)
          else h::search(h::visited)((g succ h) ::: t)
      }
      search (Nil) (List(startNode))
    }

    //println(breadthSearch(g)(4))
    println(depthSearch(g)(4))
  }
}
