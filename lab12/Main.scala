import scala.annotation.tailrec
import scala.collection.mutable._

object Main {
  def main(args: Array[String]): Unit =
  {
    zad1()
  }
  def zad1() =
  {
    val bank = new Bank()
    bank.Start()
  }
}

object Bank {
  val accountsNumber = 100
  val TransferProgramsNumber = 10
  val InitialAccountMoney = 100
  val InitialTransferNumber = 10000
}
class Bank() {
  def Start() = {
    val accounts = Array.range(0, Bank.accountsNumber).map(i => new Account("Account " + i))
    val transferPrograms = Array.range(0, Bank.TransferProgramsNumber).map(i => new TransferProgram("TP " + i, accounts))
    println("Initial sum of money: " + sumAcc(accounts));
    for (tp <- transferPrograms) tp.start()
    for (tp <- transferPrograms) tp.join()
    println(accounts.map(a=>a.money).toList)
    println("Sum of money after transfers: " + sumAcc(accounts));
  }
  def sumAcc(accounts: Array[Account]) = accounts.map(a => a.money).foldLeft(0)(_ + _)
}
class Account(val name: String, var money: Int = Bank.InitialAccountMoney) {
  val s = new java.util.concurrent.Semaphore(1)
}

class TransferProgram(val name: String, val accounts: Array[Account]) extends Thread {
  override def run(): Unit = {
    var transfers = Bank.InitialTransferNumber
    val r = new scala.util.Random
    for (i <- 0 to Bank.InitialTransferNumber) {
      var rnd1 = r.nextInt(accounts.length)
      var rnd2 = 0
      do {
          rnd2 = r.nextInt(accounts.length)
      } while (rnd1 == rnd2)

      //println(name);
      accounts(rnd1).synchronized {
        accounts(rnd1).money += 1
      }
      accounts(rnd2).synchronized {
        accounts(rnd2).money -= 1
      }
      //println(accounts(rnd1).money)
      //println(accounts(rnd2).money)
    }
  }
}

