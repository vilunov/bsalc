package tf

import scala.annotation.tailrec

sealed trait Expr2[Env, T]
object Expr2 {
  case class B[Env](b: Boolean) extends Expr2[Env, Boolean]
  case class V[Env, T](v: Var2[Env, T]) extends Expr2[Env, T]
  case class L[A, Env, B](e: Expr2[(A, Env), B]) extends Expr2[Env, A => B]
  case class A[Env, A, B](e: Expr2[Env, A => B], r: Expr2[Env, A]) extends Expr2[Env, B]
}

sealed trait Var2[Env, T]
object Var2 {
  case class VZ[Env, T]() extends Var2[(T, Env), T]
//  case object VZ extends Var2[Any, Nothing]
  case class VS[A, Env, T](v: Var2[Env, T]) extends Var2[(A, Env), T]
}

object App3 extends App {
  import Expr2._
  import Var2._

  type EnvP = (Boolean, ())

  val v = V[EnvP, Boolean](VZ())
  val ti1: Expr2[(), Boolean] = A(L(v), B(true))

  def eval[Env, T](env: Env, e: Expr2[Env, T]): T = e match {
    case V(v) => lookp(v, env)
    case B(b) => b
    case L(e) => (x => eval((x, env), e))
    case A(e, r) => eval(env, e).apply(eval(env, r))
  }

  def lookp[Env, T](v: Var2[Env, T], env: Env): T = v match {
    case VZ() => env._1
    case VS(v) => lookp(v, env._2)
  }

  println(eval((), ti1))
}

trait Symantics[Repr[env, t]] {
  def int[Env](i: Int): Repr[Env, Int]
  def add[Env](l: Repr[Env, Int], r: Repr[Env, Int]): Repr[Env, Int]

  def z[Env, A]: Repr[(A, Env), A]
  def s[Env, A, B](r: Repr[Env, A]): Repr[(B, Env), A]
  def lam[Env, A, B](r: Repr[(A, Env), B]): Repr[Env, A => B]
  def app[Env, A, B](r: Repr[Env, A => B], l: Repr[Env, A]): Repr[Env, B]
}

trait R[Env, A] {
  def apply(env: Env): A
}
trait S[Env, A] {
  def apply(i: Int): String
}

object R {
  implicit val symantics: Symantics[R] = new Symantics[R] {
    override def int[Env](i: Int): R[Env, Int] =
      _ => i

    override def add[Env](l: R[Env, Int], r: R[Env, Int]): R[Env, Int] =
      i => l(i) + r(i)

    override def z[Env, A]: R[(A, Env), A] =
      { case (a, _) => a }

    override def s[Env, A, B](r: R[Env, A]): R[(B, Env), A] =
      { case (_, env) => r(env) }

    override def lam[Env, A, B](r: R[(A, Env), B]): R[Env, A => B] =
      x => a => r((a, x))

    override def app[Env, A, B](r: R[Env, A => B], l: R[Env, A]): R[Env, B] =
      i => r(i).apply(l(i))
  }
}

object S {
  implicit val symantics: Symantics[S] = new Symantics[S] {
    override def int[Env](i: Int): S[Env, Int] =
      _ => i.toString

    override def add[Env](l: S[Env, Int], r: S[Env, Int]): S[Env, Int] =
      h => s"(${l(h)} + ${r(h)})"

    override def z[Env, A]: S[(A, Env), A] =
      h => s"x${h-1}"

    override def s[Env, A, B](r: S[Env, A]): S[(B, Env), A] =
      h => r(h-1)

    override def lam[Env, A, B](r: S[(A, Env), B]): S[Env, A => B] = { h =>
      val x = s"x$h"
      s"""($x -> ${r(h + 1)} )"""
    }

    override def app[Env, A, B](r: S[Env, A => B], l: S[Env, A]): S[Env, B] = { h =>
      s"""(${r(h)} ${l(h)})"""
    }
  }
}

object App4 extends App {
  def td1[Repr[_, _], Env](implicit a: Symantics[Repr]): Repr[Env, Int] = {
    import a._
    add(int(1), int(2))
  }

  def td2o[Repr[_, _], Env](implicit a: Symantics[Repr]): Repr[(Int, Env), Int => Int] = {
    import a._
    lam(add(z, s(z)))
  }

  def td3[Repr[_, _], Env](implicit a: Symantics[Repr]): Repr[Env, (Int => Int) => Int] = {
    import a._
    lam(add(app(z, int(1)), int(2)))
  }

  val kek = td2o[R, Unit].apply((3, ())).apply(4)
  println(kek)

  def view[T](v: S[Unit, T]): String = v(0)

  println(view(td3[S, Unit]))
}

trait Symantics2[F[_]] {
  def int(i: Int): F[Int]
  def add(l: F[Int], r: F[Int]): F[Int]
  def lam[A, B](f: F[A] => F[B]): F[A => B]
  def app[A, B](f: F[A => B], l: F[A]): F[B]
}

case class R2[T](get: T)
object R2 {
  implicit val symantics2: Symantics2[R2] = new Symantics2[R2] {
    override def int(i: Int): R2[Int] = R2(i)

    override def add(l: R2[Int], r: R2[Int]): R2[Int] = R2(l.get + r.get)

    override def lam[A, B](f: R2[A] => R2[B]): R2[A => B] = R2(i => f(R2(i)).get)

    override def app[A, B](f: R2[A => B], l: R2[A]): R2[B] = R2(f.get(l.get))
  }
}

object App5 extends App {
  def th1[F[_]](implicit a: Symantics2[F]): F[Int] = {
    import a._
    add(int(1), int(2))
  }
  def th2[F[_]](implicit a: Symantics2[F]): F[Int => Int] = {
    import a._
    lam(x => add(x, x))
  }
  def th3[F[_]](implicit a: Symantics2[F]): F[(Int => Int) => Int] = {
    import a._
    lam(x => add(app(x, int(1)), int(2)))
  }

  println(th1[R2].get)
  println(th2[R2].get(3))
  println(th3[R2].get(_ * 6))
}