package mathgraph.frontend
import mathgraph.util._

object Tokens {
  // Those are all the tokens of the language
  sealed abstract class Token extends Positioned
  case class IdToken(name: String) extends Token
  case class OpToken(name: String) extends Token
  case class KwToken(chars: String) extends Token
  case class DelimToken(chars: String) extends Token
  case class SpaceToken() extends Token
  case class CommentToken() extends Token
  case class EOFToken() extends Token
  case class ErrorToken(error: String) extends Token

  // Those are the token kinds used by Scallion
  sealed abstract class TokenKind(text: String)
  case object IdKind extends TokenKind("<id>")
  case object OpKind extends TokenKind("<op>")
  case class KwKind(chars: String) extends TokenKind(chars)
  case class DelimKind(chars: String) extends TokenKind(chars)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")

  // This retrieves the kind of a token for Scallion
  def kindOf(token: Token): TokenKind = token match {
    case IdToken(_)        => IdKind
    case OpToken(_)        => OpKind
    case KwToken(chars)    => KwKind(chars)
    case DelimToken(chars) => DelimKind(chars)
    case EOFToken()        => EOFKind
    case _                 => NoKind
  }
}
