package mathgraph.repl
import mathgraph.util._
import scala.util.parsing.input._

object CommandTokens {
  // Those are all the tokens of the language
  abstract class CommandToken extends Positional
  case class IntegerToken(name: String) extends CommandToken
  case class KeywordToken(name: String) extends CommandToken
  case object UnknownToken extends CommandToken

  // This is a reader of tokens, used by the parser
  class TokenReader(tokens: Seq[CommandToken]) extends Reader[CommandToken] {
    override def first: CommandToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = if (atEnd) NoPosition else first.pos
    override def rest: Reader[CommandToken] = new TokenReader(tokens.tail)
  }
}
