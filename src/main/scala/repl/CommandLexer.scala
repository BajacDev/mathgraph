package mathgraph.repl
import mathgraph.util._
import CommandTokens._
import silex._

object CommandLexer extends Lexers {

  type Character = Char
  type Position = SourcePosition
  type Token = CommandTokens.Token

  val lexer = Lexer(
    // Int literals
    many1(elem(_.isDigit)) |> { (cs, range) =>
      try {
        IntToken(cs.mkString.toInt).setPos(range._1)
      } catch {
        case e: NumberFormatException =>
          ErrorToken(cs.mkString).setPos(range._1)
      }
    },
    // String literals
    many1(elem(!_.isWhitespace)) |> { (cs, range) =>
      StringToken(cs.mkString).setPos(range._1)
    },
    // Whitespace
    many1(elem(_.isWhitespace)) |> SpaceToken()
  ).onError { (cs, range) =>
    ErrorToken(cs.mkString).setPos(range._1)
  }

  def apply(input: String): Seq[Token] = {
    lexer
      .spawn(
        Source.fromString(input, SourcePositioner(StringSource("", input)))
      )
      .filter {
        case SpaceToken() => false
        case _            => true
      }
      .toSeq
  }
}
