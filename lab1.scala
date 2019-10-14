object Main {
  def main(args: Array[String]): Unit =
  {
    zad3()
  }
  def zad2() =
  {
    def getN[A](xs: List[A], n: Int):A =
      if (xs.isEmpty) throw new Exception("Poza zakresem")
      else if (n == 0) xs.head
      else getN(xs.tail, n-1)

    println(getN(List(1, 3, 5, 7), 2))
//    println(getN(List(), 2))
//    println(getN(List(1), 1))
    println(getN(List(1), 0))
  }
  def zad3() =
  {
    def pairChange[A](xs: List[A]):List[A] =
      if (xs.isEmpty) List()
      else if (xs.tail.isEmpty) List(xs.head)
      else xs.tail.head :: xs.head :: pairChange(xs.tail.tail)

    println(pairChange(List(1,3,2,5,4)))
  }
}
