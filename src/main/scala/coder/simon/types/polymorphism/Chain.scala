package coder.simon.types.polymorphism

object Chain extends App {
  trait A {
    def aaa(): Unit
  }

  trait A1 extends A {
    abstract override def aaa(): Unit = {
      println("before A1")
      super.aaa()
      println("after A1")
    }
  }

  trait A2 extends A {
    abstract override def aaa(): Unit = {
      println("before A2")
      super.aaa()
      println("after A2")
    }
  }

  class A3 extends A {
    override def aaa(): Unit = {
      println("A3");
    }
  }

  object B1 extends A3 with A1 with A2
  B1.aaa()
  
  println("***********************")
  object B2 extends A3 with A2 with A1
  B2.aaa()
  
  
}