package coder.simon.types.polymorphism

import scala.language.existentials

object FBounded {

  trait Account[T <: Account[T]] {
    def addFund(amount: BigDecimal): T
  }

  class BrokerageAccount(total: BigDecimal) extends Account[BrokerageAccount] {
    override def addFund(amount: BigDecimal) = new BrokerageAccount(total + amount)
  }

  class SavingsAccount(total: BigDecimal) extends Account[SavingsAccount] {
    override def addFund(amount: BigDecimal) = new SavingsAccount(total + amount)
  }

  object Account {
    val feePercentage = BigDecimal("0.02")
    val feeThreshold = BigDecimal("10000,00")

    def deposit[T <: Account[T]](amount: BigDecimal, account: T): T = {
      if (amount < feeThreshold) account.addFund(amount - (amount * feePercentage))
      else account.addFund(amount)
    }

    def debitAll(amount: BigDecimal, accounts: List[T forSome { type T <: Account[T] }]): List[T forSome { type T <: Account[T] }] = {
      accounts.map(_.addFund(-amount))
    }
  }

  trait Fruit[T <: Fruit[T]] {
    final def compareTo(other: T): Boolean = true
  }

  class Apple extends Fruit[Apple]
  class Orange extends Fruit[Orange]

  val apple = new Apple
  val a2=new Apple
  val orange = new Orange
  val o2=new Orange
  
  apple.compareTo(a2)
  orange.compareTo(o2)

}