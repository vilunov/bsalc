package tf

trait FormattingSpec[Repr[_, _]] {
  def lit[A](i: String): Repr[A, A]
  def int[A]: Repr[A, Int => A]
  def char[A]: Repr[A, Char => A]
  def concat[A, B, C](b: Repr[B, C], a: Repr[A, B]): Repr[A, C]
}

trait Printer[A, B] {
  def apply(f: String => A): B
}
object Printer {
  implicit val formattingSpec: FormattingSpec[Printer] = new FormattingSpec[Printer] {
    override def lit[A](i: String): Printer[A, A] =
      k => k(i)

    override def int[A]: Printer[A, Int => A] =
      k => x => k(x.toString)

    override def char[A]: Printer[A, Char => A] =
      k => x => k(x.toString)

    override def concat[A, B, C](a: Printer[B, C], b: Printer[A, B]): Printer[A, C] =
      k => a(sa => b(sb => k(sa + sb)))
  }
}

trait Scanner[A, B] {
  def apply(input: String, b: B): Option[(A, String)]
}
object Scanner {
  implicit val formattingSpec: FormattingSpec[Scanner] = new FormattingSpec[Scanner] {
    override def lit[A](i: String): Scanner[A, A] = (input, b) => {
      if (input.startsWith(i)) {
        val leftovers = input.drop(i.length)
        Some((b, leftovers))
      } else None

    }

    override def int[A]: Scanner[A, Int => A] = (input, b) => {
      input.split(' ').toList match {
        case i :: cons =>
          i.toIntOption.map { k =>
            (b(k), " " + cons.mkString)
          }
        case Nil => None
      }
    }

    override def char[A]: Scanner[A, Char => A] = (input, b) => {
      input.toList match {
        case c :: inp => Some((b(c), inp.mkString))
        case _ => None
      }
    }

    override def concat[A, B, C](a: Scanner[B, C], b: Scanner[A, B]): Scanner[A, C] = (input, f) => {
      a(input, f).flatMap { case (cons, kek) =>
        b(kek, cons)
      }
    }
  }
}

object AppS extends App {
  def hw[Repr[_, _], T](implicit spec: FormattingSpec[Repr]): Repr[T, T] = {
    import spec._
    lit("Hello world")
  }

  def hw2[Repr[_, _], T](implicit spec: FormattingSpec[Repr]): Repr[T, Char => T] = {
    import spec._
    concat(concat(lit[Char => T]("Hello "), lit[Char => T]("world")), char[T])
  }

//  def hw3[Repr[_, _], T](implicit spec: FormattingSpec[Repr]): Repr[T, Char => Int => T] = {
//    import spec._
//    concat(concat(concat(lit("Value of "), char), lit("is")), int)
//  }

  println(hw[Printer, String].apply(identity))
  println(hw2[Printer, String].apply(identity)('/'))

  println(hw[Scanner, Int].apply("Hello world", 1))
  println(hw2[Scanner, String].apply("Hello world&", i => i.toString))
}