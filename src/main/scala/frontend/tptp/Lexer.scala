package mathgraph.frontend.tptp

import mathgraph.util._
import java.io.File
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
    elem('\'') ~ many(
      word("\\\'") | word("\\\\") | many(elem(c => c != '\\' && c != '\''))
    ) ~ elem('\'')
      |> { (cs, range) =>
        QuotedToken(cs.mkString.drop(1).dropRight(1)).setPos(range._1)
      },
    elem('\"') ~ many(
      word("\\\"") | word("\\\\") | many(elem(c => c != '\\' && c != '\"'))
    ) ~ elem('\"')
      |> { (cs, range) =>
        StringToken(cs.mkString.drop(1).dropRight(1)).setPos(range._1)
      }
  ).onError { (cs, range) =>
    ErrorToken(cs.mkString).setPos(range._1)
  }.onEnd { pos =>
    EOFToken().setPos(pos)
  }

  def apply(source: AbstractSource)(ctxt: Context): Iterator[FrontendToken] = {
    lexer
      .spawn(Source.fromIterator(source.source, SourcePositioner(source)))
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
