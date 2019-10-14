import scala.annotation.tailrec
object Main {
  def main(args: Array[String]): Unit =
  {
    zad3()
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
    val list1 = List(-2, -1, 0, 1, 2)
    val list2 = List((1, 2), (0, 1))
    val List(_, _, x1, _, _) = list1  
    val List(_, (x2, _)) = list2  

    println(x1)
    println(x2)
  }
  def zad5() =
  {
    def initSegment[A](xs: List[A], ys: List[A]):Boolean =
      xs match {
        case List() => true
        case _ => {
          xs.head == ys.head match {
            case true => initSegment(xs.tail, ys.tail)
            case false => false
          }
        }
      }

    // tests
    println(initSegment(List(), List()) == true)
    println(initSegment(List(), List(1, 2, 3)) == true)
    println(initSegment(List(1, 2), List(1, 2, 3)) == true)
    println(initSegment(List(2, 2), List(1, 2, 3)) == false)
    println(initSegment(List(1, 2, 3), List(1, 2, 3)) == true)
  }
  def zad6() =
  {
    def replaceNth[A](xs: List[A], n: Int, x: A):List[A] =
      n match {
        case 0 => x :: xs.tail
        case _ => xs.head :: replaceNth(xs.tail, n-1, x)
      }

    println(replaceNth(List('o','l','a','m','a','k','o','t','a'), 1, 's'))
  }
}
