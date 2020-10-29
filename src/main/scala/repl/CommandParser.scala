package mathgraph.repl
import mathgraph.util._
import mathgraph.repl.Commands._
import mathgraph.repl.CommandTokens._
import scala.util.parsing.combinator._
import scala.io.Source
import scala.language.implicitConversions

object CommandParser extends Parsers with Pipeline[Seq[CommandToken], Command] {

  type Elem = CommandToken

  def number: Parser[String] = accept(
    "integer",
    { case IntegerToken(n) =>
      n
    }
  )

  def keyword(str: String): Parser[String] = accept(
    "keyword",
    { case KeywordToken(`str`) =>
      str
    }
  )

  def anyKeyword: Parser[String] = {
    zeroArgKeyword | oneArgKeyword | twoArgKeyword
  }

  def more = rep1(number | anyKeyword)

  def oneArg = {
    opt(anyKeyword) ~ opt(number) ^^ {
      case None ~ Some(number) => Some(number)
      case _ ~ _ => None
    }
  }

  def twoArgs = {
    oneArg ~ oneArg ^^ {
      case Some(arg1) ~ Some(arg2) => Some((arg1, arg2))
      case _ => None
    }
  }

  def command: Parser[Command] = {
      zeroArgCommand | oneArgCommand | twoArgCommand
  }

  def zeroArgKeyword: Parser[String] = {
    keyword("help") |
      keyword("leave") |
      keyword("lse") |
      keyword("lss") |
      keyword("ls") |
      keyword("absurd") |
      keyword("fat") |
      keyword("faf") |
      keyword("dij") |
      keyword("stats") |
      keyword("proof") |
      keyword("undo") |
      keyword("clear")
  }

  def zeroArgCommand: Parser[Command] = {
    zeroArgKeyword ~ opt(more) ^^ {
      case cmd ~ Some(rest) => BadCommand(cmd, 0)
      case cmd ~ None => cmd match {
        case "help"   => Help
        case "leave"  => Leave
        case "lse"    => Lse
        case "lss"    => Lss
        case "ls"     => Ls
        case "absurd" => Absurd
        case "fat"    => FixAllTrue
        case "faf"    => FixAllFalse
        case "dij"    => Dij
        case "stats"  => Stats
        case "proof"  => Proof
        case "undo"  => Undo
        case "clear" => Clear
        case _        => UnknownCommand
      }
    }
  }

  def oneArgKeyword: Parser[String] = {
    keyword("fixn") |
      keyword("apply") |
      keyword("ctx") |
      keyword("chain")
  }

  def oneArgCommand: Parser[Command] = {
    oneArgKeyword ~ oneArg ~ opt(more) ^^ {
      case cmd ~ None ~ _ => BadCommand(cmd, 1)
      case cmd ~ Some(arg) ~ Some(rest) => BadCommand(cmd, 1)
      case cmd ~ Some(arg) ~ None => cmd match {
        case "fixn"   => FixN(arg.toInt)
        case "apply"  => Apply(arg.toInt)
        case "ctx"    => Ctx(arg.toInt)
        case "chain"  => Chain(arg.toInt)
        case _        => UnknownCommand
      }
    }
  }

  def twoArgKeyword: Parser[String] = {
    keyword("fix") |
    keyword("why")
  }

  def twoArgCommand: Parser[Command] = {
    twoArgKeyword ~ twoArgs ~ opt(more) ^^ {
      case cmd ~ None ~ _ => BadCommand(cmd, 2)
      case cmd ~ Some(args) ~ Some(rest) => BadCommand(cmd, 2)
      case cmd ~ Some(args) ~ None => cmd match {
        case "fix" => Fix(args._1.toInt, args._2.toInt)
        case "why" => Why(args._1.toInt, args._2.toInt)
        case _     => UnknownCommand
      }
    }
  }

  protected def apply(tokens: Seq[CommandToken])(ctxt: Context): Command = {
    if (tokens.contains(UnknownToken)) {
      UnknownCommand
    } else {

      phrase(command)(new TokenReader(tokens)) match {
        case Success(command, _) =>
          command

        case e: NoSuccess =>
          UnknownCommand
      }
    }
  }
}
