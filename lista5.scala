import scala.annotation.tailrec

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

object Main {
  def main(args: Array[String]): Unit =
  {
    zad1()
  }
  def lfrom (k:Int):LazyList[Int] = k#::lfrom(k+1)
  def zad1() =
  {
    def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] = {
      def lrepeatIter[A](x: A, k: Int, lxs: LazyList[A]): LazyList[A] =
        if (k==0) lxs
        else lrepeatIter(x, k-1, x #:: lxs)
      lxs match {
        case LazyList() => LazyList()
        case hd #:: tl => lrepeatIter(hd, k, LazyList()) #::: lrepeat(k)(tl)
      }
    }
    println(lrepeat(3)(LazyList(1,2,3)).force)
    println(lrepeat(3)(lfrom(0)).take(10).force)
  }
  def zad2() =
  {
    def lfib(): LazyList[Int] = {
      def lfibIter(a: Int, b: Int): LazyList[Int] =
        a #:: lfibIter(b, a+b)
      lfibIter(0, 1)
    }
    println(lfib().take(10).force)
  }
  def zad3() =
  {
    def lBreadth[A](ltree: lBT[A]): LazyList[A] = {
      def iter(xs: List[lBT[A]]): LazyList[A] = xs match {
        case Nil => LazyList()
        case LEmpty::tl => iter(tl)
        case LNode(v, bt1, bt2)::tl => v #:: iter(tl ::: List(bt1(), bt2()))
      }
      iter(List(ltree))
    }
    def lTree(n: Int): lBT[Int] = LNode(n, () => lTree(2*n), () => lTree(2*n+1))

    println(lBreadth(lTree(0)).take(30).force)
    println(lBreadth(lTree(1)).take(20).force)
    println(lBreadth(lTree(2)).take(20).force)
    println(lBreadth(lTree(3)).take(20).force)
  }
}
