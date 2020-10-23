package bsalc

import cats.{Applicative, Monad}
import cats.kernel.Monoid

import scala.annotation.tailrec
/*
sealed trait Store[Info, Key, Value] {
  def info: Info

  def withInfo(newInfo: Info): Store[Info, Key, Value]

  def valueAt(key: Key): Value

  def putValue(key: Key, value: Value): Store[Info, Key, Value]
}
object Store {
  def initialize[Info, Key, Value](info: Info, mapping: Key => Value): Store[Info, Key, Value] = ???
}

trait Task[Constraint[f[_]], Key, Value] {
  def run[F[_] : Constraint](func: Key => F[Value]): F[Value]
}

trait Tasks[Constraint[f[_]], Key, Value] {
  def apply(key: Key): Option[Task[Constraint, Key, Value]]
}

trait Build[Constraint[f[_]], Info, Key, Value] {
  def apply(tasks: Tasks[Constraint, Key, Value], key: Key, oldStore: Store[Info, Key, Value]): Store[Info, Key, Value]
}

final case class Identity[+T](unwrap: T)
object Identity {
  implicit val monad: Monad[Identity] = new Monad[Identity] {
    override def pure[A](x: A): Identity[A] = Identity(x)
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa.unwrap)
    @tailrec override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = {
      f(a).unwrap match {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Identity(value)
      }
    }
  }
}

final case class Const[+C, +I](runConst: C)
object Const {
  implicit def applicative[C](implicit monoid: Monoid[C]): Applicative[Const[C, *]] = new Applicative[Const[C, *]] {
    override def pure[A](x: A): Const[C, A] = Const(monoid.empty)
    override def ap[A, B](ff: Const[C, A => B])(fa: Const[C, A]): Const[C, B] =
      Const(monoid.combine(ff.runConst, fa.runConst))
  }
}
*/