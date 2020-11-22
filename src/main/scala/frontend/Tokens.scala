package mathgraph.frontend
import mathgraph.util._

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
  case class OperatorToken(op: String) extends Token
  case class PredicateToken(value: String) extends Token
  case class QuotedToken(value: String) extends Token
  case class StringToken(value: String) extends Token

  // Those are the token kinds used by Scallion
  sealed abstract class TokenKind(text: String)
  case object IdKind extends TokenKind("<id>")
  case class KwKind(chars: String) extends TokenKind(chars)
  case class DelimKind(chars: String) extends TokenKind(chars)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")
  // TODO not sure what kinds are really useful
  case class OperatorKind(value: String) extends TokenKind(value)
  case class PredicateKind(value: String) extends TokenKind(value)
  case class QuotedKind(value: String) extends TokenKind(value)
  case class StringKind(value: String) extends TokenKind(value)

  // This retrieves the kind of a token for Scallion
  def kindOf(token: Token): TokenKind = token match {
    case IdToken(_)            => IdKind
    case KwToken(chars)        => KwKind(chars)
    case DelimToken(chars)     => DelimKind(chars)
    case EOFToken()            => EOFKind
    case OperatorToken(value)  => OperatorKind(value)
    case PredicateToken(value) => PredicateKind(value)
    case QuotedToken(value)    => QuotedKind(value)
    case StringToken(value)    => StringKind(value)
    case _                     => NoKind
  }
}
