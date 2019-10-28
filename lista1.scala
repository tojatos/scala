object Main {
  def main(args: Array[String]): Unit =
  {
    zad1()
    zad2()
    zad3()
    zad4()
    zad5()
    zad6()
  }
  def zad1() =
  {
    def flatten[A](xss: List[List[A]]):List[A] =
      if (xss.isEmpty) List()
      else {
        xss.head ::: flatten(xss.tail)
      }
    val list1 = List(List(5,6), List(1, 2, 3))
    println(flatten(list1))
  }
  def zad2() =
  {
    def count[A](x: A, xs: List[A]):Int =
      if (xs.isEmpty) 0
      else count(x, xs.tail) + (if (x == xs.head) 1 else 0)

    println(count(1, List(3,4,1)))
    println(count(1, List(3,4,2)))
    println(count('a', List('a','l','a')))
  }
  def zad3() =
  {
    def replicate[A](x: A, n: Int):List[A] =
      if (n<=0) List()
      else x :: replicate(x, n-1)
    println(replicate("la", 3))
    println(replicate(6, 8))
  }
  def zad4() =
  {
    def sqrList: List[Int] => List[Int] = (xs: List[Int]) =>
      if (xs.isEmpty) List()
      else xs.head * xs.head :: sqrList(xs.tail)
    println(sqrList(List(2, 3, 5, -4, 0)))
  }
  def zad5() =
  {
    def palindrome[A](xs: List[A]):Boolean = xs == xs.reverse
    println(palindrome(List(2, 3, 5, -4, 0)))
    println(palindrome(List('a', 'l', 'a')))
  }
  def zad6() =
  {
    def listLength[A](xs: List[A]):Int =
      if (xs.isEmpty) 0
      else 1 + listLength(xs.tail)
    println(listLength(List(2, 3, 5, -4, 0)))
    println(listLength(List('a', 'l', 'a')))
    println(listLength(List()))
  }
}
