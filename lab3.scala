import scala.annotation.tailrec
object Main {
  def main(args: Array[String]): Unit =
  {
    zad2()
    zad3()
  }
  def zad2() =
  {
    def potega(a:Double, n:Int):Double =
      if (n == 0) 1
      else if (n == 1) a
      else if (n == 2) a*a
      else if (n % 2 == 0) potega(potega(a, n/2), 2)
      else potega(potega(a, (n-1)/2), 2) * a

    println(potega(2, 0))
    println(potega(2, 1))
    println(potega(5, 2))
    println(potega(2, 3))
    println(potega(2, 10))
    println(potega(60, 9))
  }
  def zad3() =
  {
    def potega(a:Double, n:Int):Double = {
      @tailrec
      def potegaIter(result:Double, accum:Double, x:Int):Double =
        if (x == 0) result
        else if (x%2 == 0) potegaIter(result, accum * accum, x/2)
        else potegaIter(result*accum, accum * accum, (x-1)/2)

      potegaIter(1, a, n)
    }
    println(potega(2, 0))
    println(potega(2, 1))
    println(potega(5, 2))
    println(potega(2, 3))
    println(potega(2, 10))
    println(potega(60, 9))
  }
}
