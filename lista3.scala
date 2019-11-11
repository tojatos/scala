import scala.annotation.tailrec
//import scala.language.postfixOps
object Main {
  def main(args: Array[String]): Unit =
  {
    zad5()
  }
  def zad2() =
  {
    def curry3[A,B,C,D](f: (A,B,C) => D) = (x:A) => (y:B) => (z:C) => f(x, y, z)
    def uncurry3[A,B,C,D](f: A => B => C => D) = (x:A, y:B, z:C) => f(x)(y)(z)
    def add = (x:Int) => (y:Int) => (z:Int) => x + y + z
    def mul = (x:Int, y:Int, z:Int) => x * y * z
    def mul_curry = curry3(mul)
    def add_uncurry = uncurry3(add)
    println(add(3)(4)(5))
    println(add_uncurry(3,4,5))
    println(mul(3,4,5))
    println(mul_curry(3)(4)(5))
  }
  def zad3() =
  {
    def sumProd(xs: List[Int]): (Int, Int) =
      xs.foldLeft(0,1)((a, x) => (a._1+x, a._2*x))
    println(sumProd(List(1,3,2,-2,3)))
  }
  def zad5() =
  {
    def insertSort[A](f: (A, A) => Boolean, xs: List[A]): List[A] = {
      def insert(elem: A, xs: List[A]): List[A] = xs match {
        case Nil => List(elem)
        case ys @ h::t => if (f(h, elem)) elem::ys else h::insert(elem, t)
      }

      xs.foldLeft(List[A]())((acc, elem) => insert(elem, acc))
    }
    def merge[A](f: (A, A) => Boolean, t: (List[A], List[A])): List[A] =
      t match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (xs @ h1::t1, ys @ h2::t2) =>
          if (f(h1, h2)) h2 :: merge(f, (xs, t2))
          else h1 :: merge(f, (t1, ys))
      }
    def split[A](xs: List[A]): (List[A], List[A]) = {
      def helper(n: Int, zs: List[A], ys: List[A]): (List[A], List[A]) =
        if (n == 0) (zs.reverse, ys)
        else helper(n-1, ys.head :: zs, ys.tail)
      helper(xs.length/2, Nil, xs)
    }
    def mergeSort[A](f: (A, A) => Boolean, xs: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case List(x) => List(x)
        case xs => split(xs) match {
          case (left, right) => merge(f, (mergeSort(f, left), mergeSort(f, right)))
        }
      }
    def list = List((5,0),(-8,1),(3,2),(74,3),(3,4),(54,5))
    def asc[B](a: (Int,B), b: (Int,B)): Boolean = a._1 < b._1
    def desc[B](a: (Int,B), b: (Int,B)): Boolean = a._1 > b._1
    println(insertSort(asc, list))
    println(insertSort(desc, list))
    //println(merge (((a: Int, b: Int) => a > b), (List(1,3,6,7,10), List(0,2,6,6,8,21))))
    //println(split(List(1,3,6,7,10)))
    //println(split(List(3,6,7,10)))
    //println(split(List()))
    //println(split(List(1)))
    println(mergeSort(asc, list))
    println(mergeSort(desc, list))
  }
}
