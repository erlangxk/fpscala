package coder.simon.types.polymorphism

object TypeClass extends App {

  trait Addable[T] {
    def plus(a: T, b: T): T
  }

  object Arithmetics {

    def add[A: Addable](x: A, y: A): A = {
      val addable = implicitly[Addable[A]]
      addable.plus(x, y)
    }
  }

  implicit val addableStr = new Addable[String] {
    def plus(a: String, b: String) = a + b
  }

  println(Arithmetics.add("abc", "xyz"))

}