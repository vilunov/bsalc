package bsalc

import cats.Applicative
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelloSpec extends AnyFlatSpec with Matchers {

}
/*
object HelloSpec {

  val makeBread: TaskApplicative[String, Food] = new TaskApplicative[String, Food] {
    override def run[F[_] : Applicative](func: String => F[Food]): F[Food] = ???
  }

}
*/