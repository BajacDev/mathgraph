package mathgraph.utils

object Pipe {
  // todo: make scalafmt aligns |>
  import scala.language.implicitConversions
  implicit class Piper[A](val a: A) {
    import scala.util.chaining._
    implicit def |>[B](f: (A) => B): B = a.pipe(f)
  }
}
