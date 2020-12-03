package mathgraph.frontend.mgl
import mathgraph.util._
import mathgraph.frontend.mgl.Tokens._
import mathgraph.frontend.OpTrees._ // the parser outputs trees where operators are not yet lowered
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
  val id: Syntax[Name] = accept(IdKind) { case IdToken(name) =>
    name
  }
  val idPos: Syntax[(Name, Position)] = accept(IdKind) {
    case tk @ IdToken(name) => (name, tk.pos)
  }
  implicit def delim(str: String): Syntax[Token] = elem(DelimKind(str))
  def kw(str: String): Syntax[Token] = elem(KwKind(str))
  val op: Syntax[Token] = elem(OpKind)
  val eof: Syntax[Token] = elem(EOFKind)

  // Main grammar definition
  lazy val program: Syntax[Program] =
    (many(manyDefinitions ~ ";".skip) ~ many(formula ~ ";".skip) ~ eof.skip)
      .map { case defs ~ fms =>
        Program(defs.flatten, fms)
      }

  val manyDefinitions: Syntax[Seq[Definition]] =
    (kw("let") ~ rep1sep(definition, ",")).map { case tk ~ defList =>
      defList.map(_.setPos(tk))
    }

  lazy val definition: Syntax[Definition] = normalDef | opDef

  lazy val normalDef: Syntax[Definition] = (id ~ opt(
    "(".skip ~ rep1sep(id, ",") ~ ")".skip
  ) ~ opt(":=".skip ~ formula)).map {
    case id ~ None ~ bodyOpt         => Let(id, Seq(), bodyOpt)
    case id ~ Some(params) ~ bodyOpt => Let(id, params, bodyOpt)
  }

  lazy val opDef: Syntax[Definition] =
    ("(" ~ id ~ ",".skip ~ id ~ ")".skip ~ id ~ op ~ id ~ opt(
      ":=".skip ~ formula
    )).map { case tk ~ assoc ~ prec ~ lhs ~ OpToken(op) ~ rhs ~ bodyOpt =>
      OpLet(assoc, prec, lhs, op, rhs, bodyOpt).setPos(tk)
    }

  // Expressions
  lazy val formula: Syntax[Expr] = recursive(
    many((kw("forall") | kw("exists")) ~ many1(id) ~ ".".skip) ~ operatorExpr
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

  val literal: Syntax[Expr] = (kw("true") | kw("false")).map {
    case tk @ KwToken("true")  => True.setPos(tk)
    case tk @ KwToken("false") => False.setPos(tk)
    case _                     => ???
  }

  val subFormula: Syntax[Expr] = ("(" ~ formula ~ ")".skip).map {
    case tk ~ fm => fm.setPos(tk)
  }

  val simpleExpr: Syntax[Expr] = variableOrCall | literal | subFormula

  val prefixed: Syntax[Expr] = prefixes(kw("~"), simpleExpr) {
    case (not, False)   => True.setPos(not)
    case (not, True)    => False.setPos(not)
    case (not, Not(fm)) => fm.setPos(not)
    case (not, fm)      => Not(fm).setPos(not)
  }

  val operatorExpr: Syntax[Expr] =
    (prefixed ~ many((kw("->") | op) ~ prefixed)).map { case first ~ rest =>
      // In the parser, the operator sequences are parsed in a flat way.
      // It is only in the OpsRewrite phase that they are correctly parsed given their precendences and associativies.
      val opsAndExprs = rest.map {
        case (tk @ KwToken("->")) ~ e => ("->", tk.pos, e)
        case (tk @ OpToken(op)) ~ e   => (op, tk.pos, e)
        case _                        => ???
      }
      OpSequence(first, opsAndExprs).setPos(first)
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
