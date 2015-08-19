package coder.simon.ch6

trait State[S, +A] { self =>
  def run: S => (A, S)

  def map[B](f: A => B) = new State[S, B] {
    def run = { s =>
      val (a, s2) = self.run(s)
      (f(a), s2)
    }
  }

  def flatMap[B](f: A => State[S, B]) = new State[S, B] {
    def run = { s =>
      val (a, s2) = self.run(s)
      f(a).run(s2)
    }
  }

  def get = new State[S, S] {
    def run = s => (s, s)
  }
  def set(ns: S) = new State[S, Unit] {
    def run = { _ => ((), ns) }
  }

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def map2[B, C](sb: State[S, B])(f: (A, B) => C) = flatMap { va => sb.map { vb => f(va, vb) } }

}

object State {
  def unit[S, A](a: A) = new State[S, A] {
    def run = s => (a, s)
  }

  def apply[S, A](f: S => (A, S)) = new State[S, A] {
    def run = f
  }

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = ls match {
    case Nil    => unit(Nil)
    case h :: t => h.map2(sequence(t))(_ :: _)
  }

}

object MX {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, coins: Int, candies: Int)

  def action(input: Input): State[Machine, (Int, Int)] = State { machine =>
    (input, machine) match {
      case (Coin, Machine(true, m, n)) if n > 0  => ((m + 1, n), Machine(false, m + 1, n))
      case (Turn, Machine(false, m, n)) if n > 0 => ((m, n - 1), Machine(true, m, n - 1))
      case _                                     => ((machine.coins, machine.candies), machine)
    }
  }

  def add(v: Int): State[Int, Int] = State { x => (x, x + v) }

  def simulateMachine(inputs: List[Input]): State[Machine, List[(Int, Int)]] = {
    val x = inputs.map(action)
    State.sequence(x)
  }

  def main(args: Array[String]): Unit = {
    val init = Machine(false, 7, 9)
    val actions = List(Coin, Turn, Coin, Turn, Coin)
    val (p, m) = simulateMachine(actions).run(init)
    println(p)
    println(m)

    val a2 = List(add(1), add(2), add(3))
    val r = State.sequence(a2).run(100)
    println(r)
  }
}

