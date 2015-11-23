package coder.simon.experiment

import scalaz._
import Scalaz._
object M1 {
  
  val MO=Monad[Option]
  val ML=Monad[List]
  val x=MO.bind(Some(1))(x=>Some(x+2))
  
  
  def plus(a:Int,b:Int)=a+b
  
  //val plusOpt=MO.lift2(plus)
   

  val list=List(1,2,3)
  
  def main(args:Array[String]):Unit={
    //val y=ML.sequence(list)
    //println(y)
    println(x)
    
   
  }
  
}