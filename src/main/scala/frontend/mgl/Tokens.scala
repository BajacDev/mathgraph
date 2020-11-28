package mathgraph.frontend.mgl
import mathgraph.util._
import mathgraph.frontend

object Tokens extends frontend.Tokens {

  // Those are the tokens specific to the mgl
  case class IdToken(name: String) extends Token

  // Those are the associated token kinds used by Scallion
  case object IdKind extends TokenKind("<id>")

  // This retrieves the kind of a token for Scallion
  override def kindOf(token: Token): TokenKind = token match {
    case IdToken(_) => IdKind
    case _          => super.kindOf(token)
  }
}
