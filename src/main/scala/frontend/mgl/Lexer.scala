package mathgraph.frontend.mgl
import mathgraph.util._
import java.io.File
import Tokens._
import silex._

/** A lexer takes as input a string and outputs a sequence of tokens */
object Lexer extends Lexers with Pipeline[AbstractSource, Iterator[Token]] {

  type Character = Char
  type Position = SourcePosition
  type Token = Tokens.Token

  // Those are the different classes of characters
  def idChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'
  def opChar(c: Char): Boolean = !c.isWhitespace && !"~().,;:".contains(c)

  val lexer = Lexer(
    // Keywords
    word("let") | word("~") | word("forall") | word("exists") |
      word("true") | word("false") | word("->") |> { (cs, range) =>
        KwToken(cs.mkString).setPos(range._1)
      },
    // Delimiters
    oneOf(".,;()") | word(":=") |> { (cs, range) =>
      DelimToken(cs.mkString).setPos(range._1)
    },
    // Identifiers
    many1(elem(idChar(_))) |> { (cs, range) =>
      IdToken(cs.mkString).setPos(range._1)
    },
    // Operators
    many1(elem(opChar(_))) |> { (cs, range) =>
      OpToken(cs.mkString).setPos(range._1)
    },
    // Whitespace
    many1(elem(_.isWhitespace)) |> SpaceToken(),
    // Single-line comments
    word("//") ~ many(elem(_ != '\n')) |> CommentToken(),
    // Multiline comments
    word("/*") ~ (many(
      elem(_ != '*') | many1(elem('*')) ~ elem(c => c != '/' && c != '*')
    )) ~ many1(elem('*')) ~ elem('/') |> CommentToken(),
    // Unclosed multiline comments
    word("/*") ~ (many(
      elem(_ != '*') | many1(elem('*')) ~ elem(c => c != '/' && c != '*')
    )) |> { (cs, range) =>
      ErrorToken(cs.mkString).setPos(range._1)
    }
  ).onError { (cs, range) =>
    ErrorToken(cs.mkString).setPos(range._1)
  }.onEnd { pos =>
    EOFToken().setPos(pos)
  }

  def apply(source: AbstractSource)(ctxt: Context): Iterator[Token] = {
    try {
      lexer
        .spawn(source.toSilexSource)
        .filter {
          case SpaceToken()   => false
          case CommentToken() => false
          case _              => true
        }
        .map {
          case tk @ ErrorToken(err) => ctxt.fatal(s"Invalid token: $err", tk)
          case tk                   => tk
        }
    } catch {
      case _: java.io.FileNotFoundException =>
        ctxt.fatal(s"File '${source.name}' does not exist.")
    }
  }
}
