package coder.simon.ch7

/**
 * @author simon
 */
trait Par[A] {
  def run: () => A
}

case class SequenceRun[A](val run: () => A) extends Par[A]
case class ParallelRun[A](val run: () => A) extends Par[A]

object Par {
  def unit[A](a: A): Par[A] = SequenceRun(() => a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](pa: Par[A]): A = pa match {
    case SequenceRun(run) => run()
    case ParallelRun(run) => ???
  }

  def fork[A](a: => Par[A]): Par[A] = ParallelRun(() => a.run())

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    val a: A = run(pa)
    val b: B = run(pb)
    unit(f(a, b))
  }
}