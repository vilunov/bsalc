import sbt._

object deps {

  private object versions {
    val scalaTest = "3.2.0"
    val cats = "2.2.0"
    val derevo = "0.11.5"
  }

  val scalaTest = "org.scalatest" %% "scalatest" % versions.scalaTest
  val cats = "org.typelevel" %% "cats-core" % versions.cats

  object derevo {
    val cats = "org.manatki" %% "derevo-cats" % versions.derevo
  }
}
