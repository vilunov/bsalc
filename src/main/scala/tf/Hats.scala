package tf

import derevo.cats.show
import derevo.derive

sealed trait Exp
final case class Lit(i: Int) extends Exp
final case class Neg(e: Exp) extends Exp
final case class Add(l: Exp, r: Exp) extends Exp


object App1 extends App {
  val ti1 = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

  def eval(e: Exp): Int = e match {
    case Lit(i) => i
    case Neg(e) => -eval(e)
    case Add(l, r) => eval(l) + eval(r)
  }
}

trait ExpSym[Repr] {
  def lit(i: Int): Repr
  def neg(e: Repr): Repr
  def add(l: Repr, r: Repr): Repr
}

/* [T] => ExpSym[T] => T */
trait ExpFT {
  def value[Repr: ExpSym]: Repr
}

object ExpSym {
  implicit val int: ExpSym[Int] = new ExpSym[Int] {
    override def lit(i: Int): Int = i
    override def neg(e: Int): Int = -e
    override def add(l: Int, r: Int): Int = l + r
  }

  implicit val string: ExpSym[String] = new ExpSym[String] {
    override def lit(i: Int): String = i.toString
    override def neg(e: String): String = s"(-$e)"
    override def add(l: String, r: String): String = s"($l + $r)"
  }

  def eval(e: ExpFT): Int = e.value[Int]
  def view(e: ExpFT): String = e.value[String]
}

object App2 extends App {
  // [forall a . ExpSYM a => a]
  val tfl1: List[ExpFT] = List(
    new ExpFT {
      def value[Repr](implicit i: ExpSym[Repr]): Repr = i.lit(1)
    },
    new ExpFT {
      def value[Repr](implicit i: ExpSym[Repr]): Repr =
        i.add(i.lit(1), i.lit(3))
    },
  )
  println(tfl1.map(ExpSym.eval))
}

trait MulSym[Repr] {
  def mul(l: Repr, r: Repr): Repr
}

object MulSym {
  implicit val int: MulSym[Int] = new MulSym[Int] {
    override def mul(l: Int, r: Int): Int = l + r
  }
}

sealed trait Tree
case class Leaf(i: String) extends Tree
case class Node(i: String, l: List[Tree]) extends Tree

object Tree {
  implicit val expSym: ExpSym[Tree] = new ExpSym[Tree] {
    override def lit(i: Int): Tree =
      Node("Lit", List(Leaf(i.toString)))
    override def neg(e: Tree): Tree =
      Node("Neg", List(e))
    override def add(l: Tree, r: Tree): Tree =
      Node("Add", List(l, r))
  }
}