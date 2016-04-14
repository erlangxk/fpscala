package coder.simon.typeevidence

import scalaz._
import Scalaz._
import Liskov.<~<
import Leibniz.===

object L1 extends App {

  case class Foo[A](a: A) {
    def length(implicit ev: A === String): Int = ev(a).length()
    def square(implicit ev: A <~< Int): Int = ev(a) * ev(a)
  }
  
  
  println(Foo("aaa").length)
  
  //Foo(3).getLength
  
  println(Foo(3).square)
  

}