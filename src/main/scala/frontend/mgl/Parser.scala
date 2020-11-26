package mathgraph.frontend.mgl
import mathgraph.util._
import mathgraph.frontend.Trees._
import scala.language.implicitConversions
import Tokens._
import scallion._

/** A parser takes as input a sequence of tokens and outputs a program */
object Parser extends Parsers with Pipeline[Iterator[Token], Program] {

  import Implicits._

  type Token = Tokens.Token
  type Kind = TokenKind

  override def getKind(token: Token): TokenKind = kindOf(token)

  // Helper functions for the basic atoms of the syntax
  val id: Syntax[Identifier] = accept(IdKind) { case IdToken(name) =>
    name
  }

  val idInfix: Syntax[Identifier] = accept(IdInfixKind) {
    case IdInfixToken(name) => name
  }

  val idPos: Syntax[(Identifier, Position)] = accept(IdKind) {
    case tk @ IdToken(name) => (name, tk.pos)
  }

  implicit def delim(str: String): Syntax[Token] = elem(DelimKind(str))

  def kw(str: String): Syntax[Token] = elem(KwKind(str))

  val eof: Syntax[Token] = elem(EOFKind)

  // Main grammar definition
  lazy val program: Syntax[Program] =
    (many(manyDefinitions ~ ";".skip) ~ many(formula ~ ";".skip) ~ eof.skip)
      .map { case defs ~ fms =>
        Program(defs.flatten, fms)
      }

  val manyDefinitions: Syntax[Seq[Let]] =
    (kw("let") ~ rep1sep(definition, ",")).map { case tk ~ defList =>
      defList.map(_.setPos(tk))
    }

  lazy val definition: Syntax[Let] = (id ~ opt(
    "(".skip ~ rep1sep(id, ",") ~ ")".skip
  ) ~ opt(":=".skip ~ formula)).map {
    case id ~ None ~ bodyOpt         => Let(id, Seq(), bodyOpt)
    case id ~ Some(params) ~ bodyOpt => Let(id, params, bodyOpt)
  }

  // Expressions
  lazy val formula: Syntax[Expr] = recursive(
    many((kw("forall") | kw("exists")) ~ many1(id) ~ ".".skip) ~ implies
  ).map { case qs ~ fm =>
    qs.foldRight(fm) {
      case ((tk @ KwToken("forall")) ~ ids1, Forall(ids2, acc)) =>
        Forall(ids1 ++ ids2, acc).setPos(tk)
      case ((tk @ KwToken("forall")) ~ ids, acc) => Forall(ids, acc).setPos(tk)
      case ((tk @ KwToken("exists")) ~ ids1, Exists(ids2, acc)) =>
        Exists(ids1 ++ ids2, acc).setPos(tk)
      case ((tk @ KwToken("exists")) ~ ids, acc) => Exists(ids, acc).setPos(tk)
    }
  }

  val variableOrCall: Syntax[Expr] =
    (idPos ~ opt("(".skip ~ rep1sep(formula, ",") ~ ")".skip)).map {
      case (id, pos) ~ None      => Apply(id, Seq()).setPos(pos)
      case (id, pos) ~ Some(fms) => Apply(id, fms).setPos(pos)
    }

  val infixCall: Syntax[Expr] = (prefixed ~ idInfix ~ prefixed).map { 
    case a ~ id ~ b => Apply(id, Seq(a, b)).setPos(a)
  }

  val literal: Syntax[Expr] = (kw("true") | kw("false")).map {
    case tk @ KwToken("true")  => True.setPos(tk)
    case tk @ KwToken("false") => False.setPos(tk)
    case _                     => ???
  }

  val subFormula: Syntax[Expr] = ("(" ~ formula ~ ")".skip).map {
    case tk ~ fm => fm.setPos(tk)
  }

  val simpleExpr: Syntax[Expr] = variableOrCall | literal | subFormula

  lazy val prefixed: Syntax[Expr] = (opt(kw("not")) ~ simpleExpr).map {
    case None ~ fm          => fm
    case Some(tk) ~ False   => True.setPos(tk)
    case Some(tk) ~ True    => False.setPos(tk)
    case Some(tk) ~ Not(fm) => fm.setPos(tk)
    case Some(tk) ~ fm      => Not(fm).setPos(tk)
  }

  val implies: Syntax[Expr] = rep1sep(prefixed, kw("->")).map { case fms =>
    fms.reduceRight[Expr] { case (a, b) => Implies(a, b).setPos(a) }
  }

  protected def apply(tokens: Iterator[Token])(ctxt: Context): Program = {
    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest)  => ctxt.fatal("Unexpected end of input.")
      case UnexpectedToken(tk, rest) =>
        ctxt.fatal(
          "Unexpected token: " + tk + ", expected one of: " + rest.first
            .mkString(", "),
          tk
        )
    }
  }
}
