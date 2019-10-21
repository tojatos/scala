import scala.annotation.tailrec
object Main {
  def main(args: Array[String]): Unit =
  {
    zad1()
    zad2()
  }
  def zad1() =
  {
    def polaczListy[A](xs: List[A], ys: List[A]):List[A] =
      (xs, ys) match {
        case (List(), _) => ys
        case (_, List()) => xs
        case (h::t, ys) => h :: polaczListy(ys, t)
      }
    println(polaczListy(List(), List()))
    println(polaczListy(List(1), List()))
    println(polaczListy(List(), List(2)))
    println(polaczListy(List(1,2,3,4,5), List(6,7,8,9,10)))
    println(polaczListy(List(1,2,3,4,5), List(6,7)))
    println(polaczListy(List(1,8,7), List(9,5,12,2,1)))
  }
  def zad2() =
  {
    def listForN(n: Int, m: Int):List[Int] = {
      def listIter(xs: List[Int], last: Int, n: Int, m: Int):List[Int] =
        if(m <= 0) xs
        else last :: listIter(xs, last + n, n, m - 1)
      listIter(List(), 0, n, m)
    }
    println(listForN(3, 4))
    println(listForN(6, 2))
  }
}
