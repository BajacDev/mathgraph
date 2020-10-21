package mathgraph.util

object Pipe {
  // todo: make scalafmt align |>
  import scala.language.implicitConversions
  implicit class Piper[A](val a: A) {
    implicit def |>[B](f: (A) => B): B = f(a)
  }
}
