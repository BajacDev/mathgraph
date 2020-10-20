package mathgraph.frontend
import mathgraph.util._
import scala.util.parsing.input._

object Tokens {
  // Those are all the tokens of the language
	abstract class Token extends Positional
	case class IdToken(name: String) extends Token
	case class KwToken(name: String) extends Token
	case class DelimToken(chars: String) extends Token

  // This is a reader of tokens, used by the parser
	class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
		override def first: Token = tokens.head
		override def atEnd: Boolean = tokens.isEmpty
		override def pos: Position = if (atEnd) NoPosition else first.pos
		override def rest: Reader[Token] = new TokenReader(tokens.tail)
	}
}
