package mathgraph.repl
import mathgraph.util._
import mathgraph.repl.Commands._
import mathgraph.repl.CommandTokens._
import scala.util.parsing.combinator._
import scala.io.Source
import scala.language.implicitConversions


object CommandParser extends Parsers with Pipeline[Seq[Token], Command] {

  type Elem = Token

  def index: Parser[String] = accept(
    "index",
    { case IndexToken(n) =>
      n
    }
  )

  def keyword(str: String): Parser[String] = accept(
    "keyword",
    { case KeywordToken(`str`) =>
      str
    }
  )

  def command: Parser[Command] = {
    badZeroArgCommand | badOneArgCommand | badTwoArgCommand |
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
    keyword("proof")
  }

  def zeroArgCommand: Parser[Command] = {
    zeroArgKeyword ^^ {
      case "help" => Help
      case "leave" => Leave
      case "lse" => Lse
      case "lss" => Lss
      case "ls" => Ls
      case "absurd" => Absurd
      case "fat" => FixAllTrue
      case "faf" => FixAllFalse
      case "dij" => Dij
      case "stats" => Stats
      case "proof" => Proof
      case _ => UnknownCommand
    }
  }

  def oneArgKeyword: Parser[String] = {
    keyword("fixn") |
    keyword("apply") |
    keyword("ctx") |
    keyword("chain")
  }

  def oneArgCommand: Parser[Command] = {
    oneArgKeyword ~ index ^^ {
      case "fixn" ~ index => FixN(index.toInt)
      case "apply" ~ index => Apply(index.toInt)
      case "ctx" ~ index => Ctx(index.toInt)
      case "chain" ~ index => Chain(index.toInt)
      case _ => UnknownCommand
    }
  }

  def twoArgKeyword: Parser[String] = {
    keyword("fix") |
    keyword("why")
  }

  def twoArgCommand: Parser[Command] = {
    twoArgKeyword ~ index ~ index ^^ {
      case "fix" ~ index1 ~ index2 => Fix(index1.toInt, index2.toInt)
      case "why" ~ index1 ~ index2 => Why(index1.toInt, index2.toInt)
      case _ => UnknownCommand
    }
  }

  def anyKeyword: Parser[String] = {
    zeroArgKeyword | oneArgKeyword | twoArgKeyword
  }

  def more = rep1(index | anyKeyword)

  def keywordAndMore = anyKeyword ~ rep(index | anyKeyword)

  def badZeroArgCommand: Parser[Command] = {
    zeroArgKeyword ~ more ^^ {
      case keyword ~ _ => BadCommand(keyword, 0)
    }
  }

  def badOneArgCommand: Parser[Command] = {
    oneArgKeyword ~ opt(keywordAndMore | (index ~ more)) ^^ {
      case keyword ~ _ => BadCommand(keyword, 1)
    }
  }

  def badTwoArgCommand: Parser[Command] = {
    twoArgKeyword ~ opt(
        keywordAndMore |
        index ~ keywordAndMore |
        (index ~ index ~ more)
    ) ^^ {
      case keyword ~ _ => BadCommand(keyword, 2)
    }
  }

  protected def apply(tokens: Seq[Token])(ctxt: Context): Command = {

    if(tokens.contains(UnknownToken)) {
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
