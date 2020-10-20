package mathgraph.utils

case class Monad[A](a: A) {
  def get = a
  def map[B](f: A => B) = Monad(f(a))
}
