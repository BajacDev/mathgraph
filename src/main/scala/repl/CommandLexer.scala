package mathgraph.repl
import mathgraph.util._
import mathgraph.repl.CommandTokens._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.{Try, Success, Failure}

object CommandLexer
    extends RegexParsers
    with Pipeline[String, Seq[CommandToken]] {

  val keywords = List(
    "help",
    "leave",
    "lse",
    "lss",
    "ls",
    "absurd",
    "fixn",
    "fix",
    "simplify",
    "why",
    "fat",
    "faf",
    "dij",
    "stats",
    "ctx",
    "chain",
    "proof",
    "undo",
    "clear"
  )

  def keyword: Parser[CommandToken] = positioned {
    rep1(
      acceptIf(c => !c.isWhitespace && !c.isDigit)(
        "Unexpected" + _
      )
    ) ^^ { cs =>
      val str = cs.mkString
      if (keywords.contains(str)) KeywordToken(str)
      else UnknownToken
    }
  }

  def number: Parser[CommandToken] = positioned {
    rep1(
      acceptIf(c => c.isDigit)(
        "Unexpected" + _
      )
    ) ^^ { cs =>
      val str = cs.mkString
      try {
        str.toInt
        IntegerToken(str)
      } catch {
        case e: Exception => UnknownToken
      }
    }
  }

  def token = positioned {
    number | keyword
  }

  def apply(str: String)(ctxt: Context): Seq[CommandToken] = {
    parseAll(rep(token), str) match {
      case Success(tokens, _) =>
        tokens

      case e: NoSuccess =>
        Seq(UnknownToken)
    }
  }
}
