package coder.simon.scalaz

import scalaz._
import Scalaz._

object MT1 {

  def main(args: Array[String]) = {
    type Result[+A] = String \/ Option[A]

    val result: Result[Int] = Some(2).right

    val transformed = for {
      option <- result
    } yield {
      for { value <- option } yield (value + 3).toString
    }
    
    println(transformed)

    type Error[+A] = \/[String, A]
    type Result2[A] = OptionT[Error, A]

    val result2 = 42.point[Result2]
    val transformed2 = for {
      value <- result2
    } yield (value + 3).toString
    
    println(transformed2.run)

    def positive(in: Int): \/[String, Boolean] = {
      if (in > 0) true.right else "Not Positive".left
    }

    val good = result2 flatMapF positive
    val bad = ((-3).point[Result2]) flatMapF positive
    
    println(good, bad)
  }

}