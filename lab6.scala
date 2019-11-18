import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit =
  {
    zad3()
  }
  def lpow(n: Int): LazyList[Int] = {
    def lpowIter(acc: Int): LazyList[Int] =
      acc #:: lpowIter(n*acc)
    lpowIter(1)
  }
  def zad2() =
  {
    println(lpow(3).take(10).force)
  }
  def zad3() =
  {
    def doModulo(mod: Int, lxs: LazyList[Int]): LazyList[Int] =
      lxs.map(x => x % mod)
    println(doModulo(3, LazyList(33, 34, 35, 36, 33, 0)).force)
    println(doModulo(9, lpow(3)).take(10).force)
  }
  def zad4() =
  {
    def onlyModulo(mod: Int, lxs: LazyList[Int]): LazyList[Int] =
      lxs.filter(x => x % mod == 0)
    println(onlyModulo(3, LazyList(33, 34, 35, 36, 33, 0)).force)
    println(onlyModulo(9, lpow(3)).take(10).force)
  }
  def zad5() =
  {
    def lshuffle(lxs: LazyList[Int], lys: LazyList[Int]): LazyList[Int] = {
      (lxs, lys) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), ys) => ys
        case (xs, LazyList()) => xs
        case (hd #:: tl, ys) => hd #:: lshuffle(ys, tl)
      }
    }
    println(lshuffle(LazyList(), LazyList(33, 34, 35, 36, 33, 0)).force)
    println(lshuffle(LazyList(4, 5, 234), LazyList(33, 34, 35, 36, 33, 0)).force)
    println(lshuffle(lpow(2), lpow(3)).take(10).force)
  }
}

