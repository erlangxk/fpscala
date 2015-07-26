package coder.simon.scalaz.kleisli

import scalaz._
import Scalaz._

object K1 {

  /*
   * in Scalaz there’s a special wrapper for function of type A => M[B]
   * 
   * (Kleisli[M,A,B])>=>(k2:Kleisli[M,B,C]): Kleisli[M,A,C]
   * 
   * Same M
   */

  def main(args: Array[String]): Unit = {

    val f = Kleisli { (x: Int) => (x + 1).some }
    val g = Kleisli { (y: Int) => (y * 100).some }

    val x = f >=> g
    val x2 = f andThen g

    val y = f <=< g
    val y2 = f compose g

    println(x(3))
    println(x2(3))
    
    println(y(3))
    println(y2(3))
    

  }

}