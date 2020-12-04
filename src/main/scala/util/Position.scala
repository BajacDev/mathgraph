package mathgraph.util
import scala.io.Source
import silex.{Source => SilexSource, Positioner}

// Represents an abstract source from which strings can be read
abstract class AbstractSource {
  val name: String
  def source: Source
  def toSilexSource: SilexSource[Char, SourcePosition] =
    SilexSource.fromIterator(source, SourcePositioner(this))
}

// Sources from files
case class FileSource(name: String) extends AbstractSource {
  def source: Source = Source.fromFile(name)
}

// Sources from strings
case class StringSource(name: String, input: String) extends AbstractSource {
  def source: Source = Source.fromString(input)
}

// Position in a file
abstract class Position {
  val source: AbstractSource
  val line: Int
  val col: Int
}

// Known positions
case class SourcePosition(source: AbstractSource, line: Int, col: Int)
    extends Position {
  override def toString: String = s"${source.name}:$line:$col"
}

// Unknown positions
case object NoPosition extends Position {
  val source = null
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
case class SourcePositioner(source: AbstractSource)
    extends Positioner[Char, SourcePosition] {
  override val start: SourcePosition = SourcePosition(source, 1, 1)

  override def increment(pos: SourcePosition, character: Char): SourcePosition =
    if (character == '\n') pos.copy(line = pos.line + 1, col = 1)
    else pos.copy(col = pos.col + 1)
}
