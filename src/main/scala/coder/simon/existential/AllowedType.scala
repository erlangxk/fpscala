package coder.simon.existential

import scala.language.implicitConversions

trait AllowedType[A] {
  type JavaType >: Null <: AnyRef

  def toJavaType(a: A): JavaType

  def toObject(a: A): Object = toJavaType(a)

}

object AllowedType {
  def apply[A](implicit ev: AllowedType[A]) = ev

  def mkInstance[A, J >: Null <: AnyRef](f: A => J): AllowedType[A] = new AllowedType[A] {
    type JavaType = J
    override def toJavaType(a: A): J = f(a)
  }

  implicit val intInstance: AllowedType[Int] = mkInstance(Int.box(_))
  implicit val strInstance: AllowedType[String] = mkInstance(identity)
  implicit val boolInstance: AllowedType[Boolean] = mkInstance(Boolean.box(_))
  implicit def optionInst[A](implicit ev: AllowedType[A]): AllowedType[Option[A]] =
    mkInstance[Option[A], ev.JavaType](s => s.map(ev.toJavaType(_)).orNull)
}

sealed trait AnyAllowedType {
  type A
  val value: A
  val evidence: AllowedType[A]
}

final case class MkAnyAllowedType[A0](value: A0)(implicit val evidence: AllowedType[A0]) extends AnyAllowedType {
  type A = A0
}

object Example {
  def bind(objs: Object*): Unit = ()

  def safeBind(objs: AnyAllowedType*): Unit = bind(objs.map(aat => aat.evidence.toObject(aat.value)): _*)

  safeBind(MkAnyAllowedType(1), MkAnyAllowedType("Hello"))

  class User

  implicit def toAnyAllowedType[A0: AllowedType](value: A0): AnyAllowedType = MkAnyAllowedType(value)

  safeBind(1, "Hello")
}