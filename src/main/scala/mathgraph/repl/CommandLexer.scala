package mathgraph.repl
import mathgraph.util._
import mathgraph.repl.CommandTokens._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.{Try, Success, Failure}


object CommandLexer extends RegexParsers with Pipeline[String, Seq[Token]] {

  val keywords = Set("help", "leave", "lse", "lss", "ls", "absurd", "fixn", "fix", "apply", "why", "fat", "faf", "dij", "stats", "chain", "proof")

  def keyword: Parser[Token] = positioned {
    rep1(
      acceptIf(c => !c.isWhitespace)(
        "Unexpected" + _
      )
    ) ^^ { cs =>
      val str = cs.mkString
      if (keywords.contains(str)) KeywordToken(str)
      else UnknownToken
    }
  }

  def number: Parser[Token] = positioned {
    rep1(
      acceptIf(c => c.isDigit)(
        "Unexpected" + _
      )
    ) ^^ { cs =>
      val str = cs.mkString
      try {
        str.toInt
        IndexToken(str)
      } catch {
        case e: Exception => UnknownToken
      }
    }
  }

  def token = positioned {
    number | keyword
  }

  def apply(str: String)(ctxt: Context): Seq[Token] = {
    parseAll(rep(token), str) match {
      case Success(tokens, _) =>
        tokens

      case e: NoSuccess =>
        Seq(UnknownToken)
    }
  }
}
