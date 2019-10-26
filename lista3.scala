import scala.annotation.tailrec
//import scala.language.postfixOps
object Main {
  def main(args: Array[String]): Unit =
  {
    zad3()
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
}
