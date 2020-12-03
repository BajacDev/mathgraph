package mathgraph.util
import silex._

// Position in a file
abstract class Position {
  val file: String
  val line: Int
  val col: Int
}

// Known positions
case class SourcePosition(file: String, line: Int, col: Int)
    extends Position {
  override def toString: String = s"$file:$line:$col"
}

// Unknown positions
case object NoPosition extends Position {
  val file = null
  val line = 0
  val col = 0
  override def toString: String = "?:?"
}

// Something that has a position
trait Positioned {
  protected var _pos: Position = NoPosition
  def pos = _pos

  def setPos(other: Positioned): this.type = setPos(other.pos)
  def setPos(pos: Position): this.type = {
    _pos = pos
    this
  }
}

// This is used by Scallion to automatically compute the position of tokens
case class SourcePositioner(file: String)
    extends Positioner[Char, SourcePosition] {
  override val start: SourcePosition = SourcePosition(file, 1, 1)

  override def increment(pos: SourcePosition, character: Char): SourcePosition =
    if (character == '\n') pos.copy(line = pos.line + 1, col = 1)
    else pos.copy(col = pos.col + 1)
}
