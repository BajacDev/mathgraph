package mathgraph.repl
import mathgraph.util._

object CommandTokens {
  // Those are all the tokens of the language
  abstract class Token extends Positioned
  case class IntToken(value: Int) extends Token
  case class StringToken(value: String) extends Token
  case class ErrorToken(error: String) extends Token
  case class SpaceToken() extends Token
}
