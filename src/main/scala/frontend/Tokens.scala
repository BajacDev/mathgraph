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

  // Those are the additional tokens of the tptp language
  case class OperatorToken(op: String) extends Token
  case class PredicateToken(value: String) extends Token
  case class SingleQuotedToken(value: String) extends Token
  case class DistinctObjectToken(value: String) extends Token
  case class DollarWordToken(value: String) extends Token
  case class WordToken(value: String) extends Token
  case class RealToken(value: String) extends Token
  case class UnsignedToken(value: String) extends Token
  case class SignedToken(value: String) extends Token

  // Those are the token kinds used by Scallion
  sealed abstract class TokenKind(text: String)
  case object IdKind extends TokenKind("<id>")
  case class KwKind(chars: String) extends TokenKind(chars)
  case class DelimKind(chars: String) extends TokenKind(chars)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")
  case object SpaceKind extends TokenKind("<whitespace>")

  case class OperatorKind(value: String) extends TokenKind(value)
  case class PredicateKind(value: String) extends TokenKind(value)
  case object SingleQuotedKind extends TokenKind("single_quoted")
  case object DistinctObjectKind extends TokenKind("<distinct_object>")
  case object DollarWordKind extends TokenKind("<dollar_dollar_word>")
  case object LowerWordKind extends TokenKind("<lower_word>")
  case object UpperWordKind extends TokenKind("<Upper_word>")
  case object UnsignedKind extends TokenKind("<unsigned_integer>")
  case object SignedKind extends TokenKind("<signed_integer>")
  case object RealKind extends TokenKind("<real>")

  // This retrieves the kind of a token for Scallion
  def kindOf(token: Token): TokenKind = token match {
    case IdToken(_)        => IdKind
    case KwToken(chars)    => KwKind(chars)
    case DelimToken(chars) => DelimKind(chars)
    case EOFToken()        => EOFKind
    case SpaceToken()      => SpaceKind

    case OperatorToken(value)                        => OperatorKind(value)
    case PredicateToken(value)                       => PredicateKind(value)
    case SingleQuotedToken(_)                        => SingleQuotedKind
    case DistinctObjectToken(_)                      => DistinctObjectKind
    case DollarWordToken(_)                          => DollarWordKind
    case WordToken(value) if value.charAt(0).isLower => LowerWordKind
    case WordToken(value) if value.charAt(0).isUpper => UpperWordKind
    case RealToken(value)                            => RealKind
    case UnsignedToken(value)                        => UnsignedKind
    case SignedToken(value)                          => SignedKind

    case _ => NoKind
  }
}

/*
  number
  lower_word upper_word dollar_dollar_word => WordKind
  single_quoted distinct_object
 */
