package coder.simon.types.phantom

/**
 * @author simon
 */
object P3 {
  sealed trait State
  final class Empty private () extends State
  final class Ready private () extends State

  class Dinner[Oeuvre <: State, MainDish <: State, Dessert <: State]() {
    def cookSalad = new Dinner[Ready, MainDish, Dessert]
    def cookSoup = new Dinner[Ready, MainDish, Dessert]
    def cookSteak = new Dinner[Oeuvre, Ready, Dessert]
    def cookCake = new Dinner[Oeuvre, MainDish, Ready]

    def serve2[T1 >: Oeuvre <: Ready, T2 >: MainDish <: Ready, T3 >: Dessert <: Ready]() = println("Now you can eat")
    
    def serve() = println("you can eat???")
  }

  object Dinner {
    def start = new Dinner[Empty, Empty, Empty]
  }

  def main(args: Array[String]): Unit = {
    Dinner.start.serve()
    Dinner.start.cookSalad.cookCake.serve()

    val x = Dinner.start.cookSoup.cookSteak.cookCake

    x.serve2()

  }

}