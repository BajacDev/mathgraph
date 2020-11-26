package mathgraph.frontend.tptp

import mathgraph.util._
import java.io.File
import scala.util.Try
import mathgraph.frontend.Tokens.{Token => FrontendToken, _}
import silex._

/** A lexer takes as input a string and outputs a sequence of tokens */
object Lexer
    extends Lexers
    with Pipeline[AbstractSource, Iterator[FrontendToken]] {

  type Character = Char
  type Position = SourcePosition
  type Token = FrontendToken

  val lexer = Lexer(
    // Keywords
    word("fof") | word("cnf") | word("include")
      |> { (cs, range) => KwToken(cs.mkString).setPos(range._1) },
    // Punctuation
    oneOf("(),.[]:")
      |> { (cs, range) => DelimToken(cs.mkString).setPos(range._1) },
    // Whitespace
    many1(elem(_.isWhitespace)) |> SpaceToken(),
    // Operators
    oneOf("!?~&|") | word("<=>") | word("<=") | word("=>") |
      word("<~>") | word("~|") | word("~&")
      |> { (cs, range) => OperatorToken(cs.mkString).setPos(range._1) },
    //Predicates
    elem('=') | word("!=") | word("$true") | word("$false")
      |> { (cs, range) => PredicateToken(cs.mkString).setPos(range._1) },
    // Single line comments
    elem('%') ~ many(elem(_ != '\n'))
      |> { (cs, range) => CommentToken().setPos(range._1) },
    word("/*") ~ many(
      many(elem(_ != '*')) ~ many1(elem('*')) ~ many(
        elem(c => c != '*' && c != '/')
      )
    ) ~ many(elem(_ != '*')) ~ many1(elem('*')) ~ elem('/')
      |> { cs => CommentToken() },
    opt(oneOf("+-")) ~ (
      elem('0') | (elem(c => c.isDigit && c != '0') ~ many(
        elem(_.isDigit)
      )) //Unsigned
    ) ~ opt(
      elem('.') ~ many1(elem(_.isDigit)) //Fraction
    )
      |> { (cs, range) =>
        {
          val value = cs.mkString

          try {

            if (value.toInt < 0) SignedToken(value).setPos(range._1)
            else UnsignedToken(value).setPos(range._1)

          } catch {
            case e: NumberFormatException =>
              if (Try(value.toDouble).isSuccess)
                RealToken(value).setPos(range._1)
              else ErrorToken(value).setPos(range._1)
          }
        }
      },
    elem('\'') ~ many(
      word("\\\'") | word("\\\\") | many(elem(c => c != '\\' && c != '\''))
    ) ~ elem('\'')
      |> { (cs, range) =>
        SingleQuotedToken(cs.mkString.drop(1).dropRight(1)).setPos(range._1)
      },
    elem('\"') ~ many(
      word("\\\"") | word("\\\\") | many(elem(c => c != '\\' && c != '\"'))
    ) ~ elem('\"')
      |> { (cs, range) =>
        DistinctObjectToken(cs.mkString.drop(1).dropRight(1)).setPos(range._1)
      },
    elem(_.isLetter) ~ many(elem(c => c.isLetter || c.isDigit || c == '_'))
      |> { (cs, range) =>
        WordToken(cs.mkString).setPos(range._1)
      },
    word("$$") ~ elem(_.isLower) ~ many(
      elem(c => c.isLetter || c.isDigit || c == '_')
    )
      |> { (cs, range) =>
        DollarWordToken(cs.mkString.drop(2)).setPos(range._1)
      }
  ).onError { (cs, range) =>
    ErrorToken(cs.mkString).setPos(range._1)
  }.onEnd { pos =>
    EOFToken().setPos(pos)
  }

  def apply(source: AbstractSource)(ctxt: Context): Iterator[FrontendToken] = {
    
    val lexerOutput = lexer(
      Source.fromIterator(source.source, SourcePositioner(source))
    )

    lexerOutput
      .filter {
        case SpaceToken()   => false
        case CommentToken() => false
        case _              => true
      }
      .map {
        case tk @ ErrorToken(err) => ctxt.fatal(s"Invalid token: $err", tk)
        case tk                   => tk
      }
  }
}
