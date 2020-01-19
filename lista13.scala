import scala.collection.mutable._

object Main {

  def main(args: Array[String]): Unit =
  {
    zad5()
  }
  def zad5() = {
    def wordCounter(text: String): Map[String, Int] = {
      Map() ++ text.split(' ').groupBy(i => i).view.mapValues(_.size)
    }
    println(wordCounter("Ala ma kota Ala hej ho hej ho hej sokoły"))

  }
}

class Point(var x:Double = 0.0, var y:Double = 0.0) {
  override def toString = "[" + x + ", " + y + "]"
}
