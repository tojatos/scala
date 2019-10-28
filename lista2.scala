import scala.annotation.tailrec
object Main {
  def main(args: Array[String]): Unit =
  {
    zad5()
  }
  def zad2() =
  {
    def fib(n: Int):Int =
      n match {
        case 0 => 0
        case 1 => 1
        case _ => fib(n-2) + fib(n-1)
      }
    def fibTail(n: Int):Int = {
      @tailrec
      def fibIter(n: Int, x1: Int, x2: Int):Int =
        if (n<=1) x2 else fibIter(n-1, x2, x1+x2)
      fibIter(n, 0, 1)
    }
      
    println(fibTail(42))
    println(fib(42))
  }
  def zad3() =
  {
    def root3(a: Double):Double = {
      @tailrec
      def root3Iter(accum: Double):Double =
        if (math.abs(accum*accum*accum - a) <= 1.0e-15 * math.abs(a)) accum
        else root3Iter(accum + (a/(accum*accum) - accum)/3)
        root3Iter(if(a>1) a/3 else a)
    }
    println(root3(8))
    println(root3(7))
  }
  def zad4() =
  {
    val List(_, _, x1, _, _) = List(-2, -1, 0, 1, 2)  
    val List(_, (x2, _)) = List((1, 2), (0, 1))  

    println(x1)
    println(x2)
  }
  def zad5() =
  {
    @tailrec
    def initSegment[A](xs: List[A], ys: List[A]):Boolean =
      (xs, ys) match {
        case (List(), _) => true
        case (_, List()) => false
        case (h1::t1, h2::t2) =>
          if (h1 == h2) initSegment(t1, t2) else false
      }

    // tests
    println(initSegment(List(), List()) == true)
    println(initSegment(List(5), List()) == false)
    println(initSegment(List(), List(1, 2, 3)) == true)
    println(initSegment(List(1, 2), List(1, 2, 3)) == true)
    println(initSegment(List(2, 2), List(1, 2, 3)) == false)
    println(initSegment(List(1, 2, 3), List(1, 2, 3)) == true)
  }
  def zad6() =
  {
    def replaceNth[A](xs: List[A], n: Int, x: A):List[A] =
      (xs, n) match {
        case (_::t1, 0) => x :: t1
        case (h1::t1, _) => h1 :: replaceNth(t1, n-1, x)
        case _ => throw new Exception("Invalid arguments")
      }

    println(replaceNth(List('o','l','a','m','a','k','o','t','a'), 1, 's'))
  }
}
