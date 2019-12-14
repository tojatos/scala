import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit =
  {
    zad1()
  }
  def zad1() = {
    val t1 = new Time(50)
    println(t1.hours)
    t1.hours = 4
    println(t1.hours)
    t1.hours = -34
    println(t1.hours)
    val t2 = Time(-320)
    println(t2.hours)
  }
}

class Time(time: Int) {
  private var s: Int = if (time < 0) 0 else time
  def hours: Int = s
  def hours_=(newTime: Int): Unit = {
    s = if (newTime < 0) 0 else newTime
  }
}
object Time {
  def apply(time: Int) = new Time(time)
}

