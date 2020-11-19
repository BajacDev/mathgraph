package mathgraph.frontend
import mathgraph.util._
import scala.util.parsing.input.Reader
import scala.util.parsing.input.{NoPosition => NoPos}

object Tokens {
  // Those are all the tokens of the language
  sealed abstract class Token extends Positioned
  case class IdToken(name: String) extends Token
  case class KwToken(chars: String) extends Token
  case class DelimToken(chars: String) extends Token
  case class SpaceToken() extends Token
  case class CommentToken() extends Token
  case class EOFToken() extends Token
  case class ErrorToken(error: String) extends Token

  // Those are the token kinds used by Scallion
  sealed abstract class TokenKind(text: String)
  case object IdKind extends TokenKind("<id>")
  case class KwKind(chars: String) extends TokenKind(chars)
  case class DelimKind(chars: String) extends TokenKind(chars)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")

  // This retrieves the kind of a token for Scallion
  def kindOf(token: Token): TokenKind = token match {
    case IdToken(_)        => IdKind
    case KwToken(chars)    => KwKind(chars)
    case DelimToken(chars) => DelimKind(chars)
    case EOFToken()        => EOFKind
    case _                 => NoKind
  }

  // This is a reader of tokens, used by the parser TODO: remove it when parser uses Scallion
  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: scala.util.parsing.input.Position = NoPos
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }
}
