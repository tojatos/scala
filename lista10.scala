import scala.annotation.tailrec
import scala.collection.mutable._

object Main {

  def main(args: Array[String]): Unit =
  {
    zad4()
  }
  def zad3() =
  {
    var queueOfPoints = DEQueue(new Point)
    val queueOfPoints1 = queueOfPoints.enqueue(new Pixel(1, 2))
    println(queueOfPoints1.dequeue.first.asInstanceOf[Pixel])
    val queueOfPixels = DEQueue(new Pixel(2, 3))
    val queueOfPixels1 = queueOfPixels.enqueue(new Point(1, 0))
    queueOfPoints = queueOfPixels
    println(queueOfPoints.first)
  }
  def zad4() =
  {
    // Traversable is deprecated
    def copy[A](dest: Seq[A], src: Iterable[A]): Unit =
    {
      var i = 0
      src.foreach {
        e => dest.update(i, e)
        i += 1
      }
    }

    val foo = new Stack[Int]()
    val bar = new Queue[Int]()
    foo ++= List(1, 2, 3)
    bar ++= List(4, 5)
    println(foo)
    println(bar)
    copy(foo, bar)
    println(foo)
    println(bar)
  }
}

class GenericCellImm[T] (val x: T) {}
class GenericCellImm2[+T] (val x: T) {}
class GenericCellMut[T] (var x: T) {}

// Mutable can be invariant only (as it contains getter and setter)
//class GenericCellMut2[+T] (var x: T) {}
//class GenericCellMut3[-T] (var x: T) {}

// Every function has to be of type [-A, +B]
abstract class Sequence[+A] {
  def append[S >: A](x: Sequence[S]): Sequence[S]
}

class Point(
  var x:Double = 0.0,
  var y:Double = 0.0
) {
  override def toString = "[" + x + ", " + y + "]"
}

import java.awt.Color

class Pixel(
  xp:Double=0.0,
  yp:Double=0.0,
  var color:Color = Color.BLACK
) extends Point(xp, yp) {
  override def toString = super.toString + " " + color
}

class EmptyException(msg: String) extends Exception(msg)

trait MyQueue[+T] {
  def enqueue[S >: T](x: S): MyQueue[S]
  def dequeue: MyQueue[T]
  def first: T
  def isEmpty: Boolean
}

object DEQueue {
  def apply[T](xs: T*): MyQueue[T] = new DeQueueImpl[T]((xs.toList.reverse, Nil))
  def empty[T]: MyQueue[T] = new DeQueueImpl[T]((Nil,Nil))

  private class DeQueueImpl[T](private val rep: (List[T],List[T])) extends MyQueue[T] {
    def enqueue[S >: T](x: S) = rep match {
      case (Nil, Nil) => new DeQueueImpl((List(x), Nil))
      case (xl, yl) => new DeQueueImpl((xl, x::yl))
    }
    def dequeue = rep match {
      case (_::t, yl) => t match {
        case Nil => new DeQueueImpl((yl.reverse, Nil))
        case _ => new DeQueueImpl((t, yl))
      }
      case _ => this
    }
    def first = rep match {
      case (h::_, _) => h
      case _ => throw new EmptyException("Empty queue")
    }
    def isEmpty = rep == (Nil, Nil)
  }
}
