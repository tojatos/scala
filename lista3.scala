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
    def list = List((5,0),(-8,1),(3,2),(74,3),(3,4),(54,5))
    def asc[B](a: (Int,B), b: (Int,B)): Boolean = a._1 < b._1
    def desc[B](a: (Int,B), b: (Int,B)): Boolean = a._1 > b._1
    println(insertSort(asc, list))
    println(insertSort(desc, list))
  }
}
