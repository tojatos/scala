import scala.collection.mutable._

object Main {

  def main(args: Array[String]): Unit =
  {
    zad1()
  }
  def zad1() = {
    val a = new Pair(5, "Ala")
    println(a)
  }
  def zad5() = {
    def wordCounter(text: String): Map[String, Int] = {
      Map() ++ text.split(' ').groupBy(i => i).view.mapValues(_.size)
    }
    println(wordCounter("Ala ma kota Ala hej ho hej ho hej sokoły"))
  }
}

class Pair[A,B](var fst: A, var snd: B)
{
  override def toString = s"($fst, $snd)"
}

class Point(var x: Double = 0.0, var y: Double = 0.0) {
  override def toString = "[" + x + ", " + y + "]"
}
//class Circle(override var x: Double = 0.0, override var y: Double = 0.0) extends Point
