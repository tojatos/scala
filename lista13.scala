import scala.collection.mutable._

object Main {

  def main(args: Array[String]): Unit =
  {
    zad3()
  }
  def zad1() = {
    val a = new Pair(5, "Ala")
    val b = new AbstractPair {
      type A = Int
      type B = String
      var fst = 5
      var snd = "Ala"
    }
    println(a)
    println(b)
    //scala> a
    //res0: Pair[Int,String] = (5, Ala)
    //
    //scala> b
    //res1: AbstractPair{type A = Int; type B = String} = (5, Ala)
  }
  def zad3() = {
    val a = new Pracownik("Nowak")
    println(a)
    println(a.liczbaPracownikow)
    val b = new Pracownik("Kowalski")
    println(a)
    println(b)
    println(a.liczbaPracownikow)
    println(b.liczbaPracownikow)
    a.zwolnij
    println(a)
    println(b)
    println(a.liczbaPracownikow)
    println(b.liczbaPracownikow)
    b.zwolnij
    println(a)
    println(b)
    println(a.liczbaPracownikow)
    println(b.liczbaPracownikow)
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

trait AbstractPair {
  type A
  type B
  var fst: A
  var snd: B
  override def toString = s"($fst, $snd)"
}

class Pracownik(var nazwisko: String) {
  Pracownik.liczbaPracownikow += 1
  private[this] var zwolniony: Boolean = false
  def liczbaPracownikow = Pracownik.liczbaPracownikow
  def zwolnij = {
    Pracownik.liczbaPracownikow -= 1
    zwolniony = true
  }
  override def toString = s"$nazwisko, zwolniony: $zwolniony"
}
object Pracownik {
  private var liczbaPracownikow = 0
}

class Point(var x: Double = 0.0, var y: Double = 0.0) {
  override def toString = "[" + x + ", " + y + "]"
}

class Circle(x: Double, y: Double, var r: Double = 0.0) extends Point(x, y)
class Cylinder(x: Double, y: Double, r: Double, var h: Double = 0.0) extends Circle(x, y, r)
