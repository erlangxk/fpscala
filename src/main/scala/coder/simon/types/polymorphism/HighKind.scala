package coder.simon.types.polymorphism

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

object HighKind {

  trait Iterable[T] {
    def filter(p: T => Boolean): Iterable[T]
    def remove(p: T => Boolean): Iterable[T] = filter(x => !p(x))
  }

  trait List[T] extends Iterable[T] {
    def filter(p: T => Boolean): List[T]
    override def remove(p: T => Boolean): List[T] = filter(x => !p(x))
  }
}
object HighKind2 {

  trait Iterable[T, Container[_]] {
    def filter(p: T => Boolean): Container[T]
    def remove(p: T => Boolean): Container[T] = filter(x => !p(x))
  }

  trait List[T] extends Iterable[T, List]
}

object Collection {
  trait Builder[Container[_], T] {
    def +=(el: T): Unit
    def toC(): Container[T]
  }

  trait Iterator[T] {
    def next(): T
    def hasNext: Boolean
    final def foreach(op: T => Unit): Unit = while (hasNext) op(next())
  }

  trait Buildable[Container[_]] {
    def build[T]: Builder[Container, T]
    final def buildWith[T](f: Builder[Container, T] => Unit): Container[T] = {
      val b = build[T]
      f(b)
      b.toC()
    }
  }

  trait Iterable[T] {

    type Container[X] <: Iterable[X]

    def elements: Iterator[T]

    final def mapTo[U, C[_]](f: T => U)(b: Buildable[C]): C[U] = {
      val buff = b.build[U]
      elements.foreach { e => buff += f(e) }
      buff.toC()
    }

    final def filterTo[C[_]](p: T => Boolean)(b: Buildable[C]): C[T] = b.buildWith[T] { buff =>
      elements.foreach { e => if (p(e)) buff += e }
    }

    final def flatMapTo[U, C[_]](f: T => Iterable[U])(b: Buildable[C]): C[U] = {
      val buff = b.build[U]
      elements.foreach { oe => f(oe).elements.foreach { ie => buff += ie } }
      buff.toC()
    }

    def map[U](f: T => U)(b: Buildable[Container]) = mapTo[U, Container](f)(b)

    def filter(p: T => Boolean)(b: Buildable[Container]) = filterTo[Container](p)(b)

    def flatMap[U](f: T => Iterable[U])(b: Buildable[Container]) = flatMapTo[U, Container](f)(b)

  }

  object ListBuildable extends Buildable[List] {
    def build[T]: Builder[List, T] = new Builder[List, T] {
      val lb = new ListBuffer[T]
      def +=(el: T) = lb += el
      def toC(): List[T] = lb.toList
    }
  }

  class Listx[T](val lb: ListBuffer[T]) extends Iterable[T] {

    type Container[X] = Listx[X]
    def elements = new Iterator[T] {
      
      def next(): T = {
        val h = lb(0)
        lb.remove(0)
        h
      }

      def hasNext: Boolean = lb.isEmpty

    }

  }

}