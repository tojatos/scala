import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit =
  {
    zad2()
  }
  def zad1() =
  {
    def whileLoop(cond: () => Boolean, expr: () => Unit): Unit =
      if (cond()) {
        expr()
        whileLoop(cond, expr)
      }
    var x = 5;
    whileLoop(() => x != 0, () => {
      println(x)
      x = x - 1;
    })
  }
  def zad2() =
  {
    def swap(tab: Array[Int], i: Int, j: Int) =
    {
      val tmp = tab(i)
      tab(i) = tab(j)
      tab(j) = tmp
    }

    def choosePivot(tab: Array[Int], m: Int, n: Int): Int =
      tab((m+n)/2)

    def partition(tab: Array[Int], l: Int, r: Int) =
    {
      var (i, j) = (l, r)
      val pivot = choosePivot(tab, l, r)
      while(i < j) {
        while (tab(i) < pivot) i = i+1
        while (pivot < tab(j)) j = j-1
        if (i <= j) {
          swap(tab, i, j)
          i = i+1
          j = j-1
        }
      }
      (i, j)
    }

    def quick(tab: Array[Int], l: Int, r: Int): Unit =
    {
      if(l < r) {
        val (i, j) = partition(tab, l, r)
        if(j-1 < r-i) {
          quick(tab, l, j)
          quick(tab, i, r)
        } else {
          quick(tab, i, r)
          quick(tab, l, j)
        }
      }
    }
    
    def quicksort(tab: Array[Int]) = quick(tab, 0, tab.length - 1)

    var arr1 = Array[Int](1, 2, 3, 4, 5)
    var arr2 = Array[Int](-1, 2, 234, 4, 2)
    swap(arr1, 0, 3)
    println(arr1.toList)
    quicksort(arr1)
    println(arr1.toList)
    quicksort(arr2)
    println(arr2.toList)

  }
}
