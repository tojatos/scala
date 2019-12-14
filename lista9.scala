import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit =
  {
    zad3()
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

  def zad2() = {
    val t1 = new Time2(20, 40)
    val t2 = new Time2(21, 37)
    println(t1.before(t2))
    println(t2.before(t1))
    t1.hour = 21
    println(t1.before(t2))
    println(t2.before(t1))
  }

  def zad3() = {
    val p1 = new Pojazd("Fbi", "Mazda", 1992, "AL43Q")
    val p2 = new Pojazd("Stary", "Opel", 1992)
    val p3 = new Pojazd("Mimi", "Audi")
    val p4 = new Pojazd("Keke", "Volvo", numer_rej="ABC")
    print(p1)
    print(p2)
    print(p3)
    print(p4)
  }

  def print(pojazd: Pojazd) = {
    println(pojazd.producent)
    println(pojazd.model)
    println(pojazd.rok)
    println(pojazd.numer_rej)
    println()
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

class Time2(hours: Int, minutes: Int) {
  require(hours >= 0 && minutes >= 0) 
  private var h: Int = hours
  private var m: Int = minutes
  def hour: Int = h
  def hour_=(newHour: Int): Unit = {
    require(newHour >= 0) 
    h = newHour
  }
  def minute: Int = m
  def minute_=(newMinute: Int): Unit = {
    require(newMinute >= 0) 
    m = newMinute
  }
  def before(other: Time2): Boolean = {
    if (other.hour == hour) other.minute > minute
    else other.hour > hour
  }
}

class Time3(time: Int) {
  private var m: Int = if (time < 0) 0 else time * 60
  def hours: Int = m/60
  def hours_=(newTime: Int): Unit = {
    m = if (newTime < 0) 0 else newTime * 60
  }
}

class Pojazd(val producent: String, val model: String, val rok: Int = -1, var numer_rej: String = "")

