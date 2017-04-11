package coder.simon.types.phantom

object P4 extends App {

  trait Runner {
    def run: Unit
  }

  trait LoggableRunner extends Runner {
    abstract override def run: Unit = {
      println("logging enter")
      super.run
      println("logging exit")
    }
  }

  class RealRunner extends Runner {
    def run = println("running...")
  }

  val a = new RealRunner with LoggableRunner

  a.run

}