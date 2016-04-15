package coder.simon.scalaz.monad

import scala.math.abs
import scalaz._
import Scalaz._

object M3 extends App {

  type Birds = Int

  case class Pole(left: Birds, right: Birds) {
    def landLeft(n: Birds): Option[Pole] =
      if (abs(left + n - right) < 4)
        copy(left = left + n).some else none
    def landRight(n: Birds) =
      if (abs(right + n - left) < 4)
        copy(right = right + n).some else none
  }
  
  def landLeft(n:Birds)(p:Pole):Option[Pole]=p.landLeft(n)
  def landRight(n:Birds)(p:Pole):Option[Pole]=p.landRight(n)
  
  val x=Kleisli(landLeft(2)) >==> landRight(2) >==> landRight(2) >==>landRight(2)
  println(x(Pole(0,0)))

}