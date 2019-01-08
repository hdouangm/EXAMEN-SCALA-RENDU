package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS



// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2
  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(i) => Option(i * 2)
    case _ => None
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = i.map(p => p * 2)

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal
  case object Cat extends Animal
  case object Bird extends Animal
  case class Dog(age: Int) extends Animal

  def formatAnimal(animal: Animal): String =
    if (animal == Cat)
      "It's a cat"
    else if (animal == Bird)
      "It's a bird"
    else if (animal.isInstanceOf[Dog])
      s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    else
      throw new RuntimeException("This should not happen but I'm a Java developer !")

  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] = {
    def indexOfAux(l: List[A], a: A, cpt: Int): Option[Int] = l match {
      case Nil => None
      case h :: _ if h == a => Some(cpt)
      case _ :: tail => indexOfAux(tail, a, cpt + 1)
    }
    indexOfAux(l, a, 0)
  }




  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)
  def keepValid[A](l: List[Either[Error, A]]): List[A] = l.collect{case Right(item) => item}

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = {
    l.foldLeft[A](empty)((a, b) => combine(a, b))
  }



  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = aggregate(keepValid(l), combine, empty)


  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)
  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A) : A
  }


  def aggregateValidM[A](l: List[Either[Error, A]],monoid : Monoid[A]):A = l match {
    case l if l.isEmpty => monoid.empty
    case _ => monoid.combine(keepValid(l).reduce((x,y) => monoid.combine(x,y)),monoid.empty)
  }

  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)

  implicit object MonoidStrings extends Monoid[String] {
    def empty = ""
    def combine(x: String, y: String) = x + y
  }

  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
  abstract class FinancialAsset {
    def computeEarnings: Double
  }

  abstract class FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  class LivretA(override val amount: Double) extends FlatRateAsset {
    override protected val rate: Double = LivretA.Rate
  }

  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }

  class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset {
    override protected val rate: Double = Pel.Rate
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }
  class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings(assets: List[FinancialAsset]): Double =
    assets.map(_.computeEarnings).sum

  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)

}
