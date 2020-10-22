package mathgraph.frontend
import mathgraph.util._
import Tokens._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

/** A lexer takes as input a string and outputs a sequence of tokens */
object Lexer extends RegexParsers with Pipeline[String, Seq[Token]] {
  // Those are all the keywords of the language
  val keywords = Set("let", "not", "forall", "exists", "->")
  val delimiters = ".,;()="

  def idOrKw: Parser[Token] = positioned {
    rep1(
      acceptIf(c => !delimiters.contains(c) && !c.isWhitespace)(
        "Unexpected" + _
      )
    ) ^^ { cs =>
      val str = cs.mkString
      if (keywords.contains(str)) KwToken(str)
      else IdToken(str)
    }
  }

  def delim: Parser[Token] = positioned {
    ("." | "," | ";" | "(" | ")" | "=") ^^ DelimToken
  }

  def token = positioned {
    idOrKw | delim
  }

  def apply(str: String)(ctxt: Context): Seq[Token] = {
    parseAll(rep(token), str) match {
      case Success(tokens, _) =>
        tokens

      case e: NoSuccess =>
        ctxt.fatal(e.msg, e.next.pos)
    }
  }
}
