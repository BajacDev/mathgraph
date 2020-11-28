package mathgraph.frontend
import mathgraph.util._

trait Tokens {
  // Those are all the tokens common to the mgl and tptp languages
  abstract class Token extends Positioned
  case class KwToken(chars: String) extends Token
  case class DelimToken(chars: String) extends Token
  case class SpaceToken() extends Token
  case class CommentToken() extends Token
  case class EOFToken() extends Token
  case class ErrorToken(error: String) extends Token

  // Those are the associated token kinds used by Scallion
  abstract class TokenKind(text: String)
  case class KwKind(chars: String) extends TokenKind(chars)
  case class DelimKind(chars: String) extends TokenKind(chars)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")

  // This retrieves the kind of a token for Scallion
  def kindOf(token: Token): TokenKind = token match {
    case KwToken(chars)    => KwKind(chars)
    case DelimToken(chars) => DelimKind(chars)
    case EOFToken()        => EOFKind
    case _                 => NoKind
  }
}
