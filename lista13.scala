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
  def zad4() = {
    val c = new Cylinder(3, 4, 5, 6)
    val p = new Point(3, 4)
    p.x_=(34)
    c.r_=(99).h_=(32)
    println(c)
    println(p)
  }
  def zad5() = {
    def wordCounter(text: String): Map[String, Int] =
      Map() ++ text.split(' ').groupBy(i => i).view.mapValues(_.size)
    //def wordCounter2(text: String) =
    //  Map() ++ text.split(' ').groupBy(i => i).view.mapValues(_.toList)
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
  def zwolnij = {
    if(!zwolniony) {
      Pracownik.liczbaPracownikow -= 1
      zwolniony = true
    }
  }
  override def toString = s"$nazwisko, zwolniony: $zwolniony"
}
object Pracownik {
  private var liczbaPracownikow = 0
  def liczbaPrac = Pracownik.liczbaPracownikow
}

class Point(protected var xt: Double = 0.0, protected var yt: Double = 0.0) {
  def x_=(newx: Double):this.type = {xt=newx; this}
  def x = xt
  def y_=(newy: Double):this.type = {yt=newy; this}
  def y = yt
  override def toString = s"[$x,$y]"
}

class Circle(xt: Double, yt: Double, protected var rt: Double = 0.0) extends Point(xt, yt) {
  def r_=(newr: Double):this.type = {rt=newr; this}
  def r = rt
  override def toString = s"[$x,$y,$r]"
}
class Cylinder(xt: Double, yt: Double, rt: Double, protected var ht: Double = 0.0) extends Circle(xt, yt, rt) {
  def h_=(newh: Double):this.type = {ht=newh; this}
  def h = ht
  override def toString = s"[$x,$y,$r,$h]"
}
