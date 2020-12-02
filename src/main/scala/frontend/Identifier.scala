package mathgraph.frontend

object Identifier {
  private val counter = new mathgraph.util.UniqueCounter[String]
  def fresh(name: String): Identifier = new Identifier(name)
}

// Denotes a unique identifier in a MGL program
final class Identifier private (val name: String) {
  // This is lazy so that identifiers are numbered in the order they are used (e.g. when printing)
  private lazy val id = Identifier.counter.next(name)

  def fullName = s"${name}_$id"
  override def toString: String = name
}
