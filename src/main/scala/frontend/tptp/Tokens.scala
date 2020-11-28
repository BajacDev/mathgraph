package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend

object Tokens extends frontend.Tokens {

  // Those are the tokens specific to the tptp language
  case class OperatorToken(op: String) extends Token
  case class PredicateToken(value: String) extends Token
  case class SingleQuotedToken(value: String) extends Token
  case class DistinctObjectToken(value: String) extends Token
  case class DollarWordToken(value: String) extends Token
  case class WordToken(value: String) extends Token
  case class RealToken(value: String) extends Token
  case class UnsignedToken(value: String) extends Token
  case class SignedToken(value: String) extends Token

  // Those are the associated token kinds used by Scallion
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
  override def kindOf(token: Token): TokenKind = token match {
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
    case _                                           => super.kindOf(token)
  }
}
