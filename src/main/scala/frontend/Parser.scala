package mathgraph.frontend
import mathgraph.util._
import mathgraph.frontend.Tokens._
import mathgraph.frontend.Trees._
import scala.util.parsing.combinator._
import scala.io.Source
import scala.language.implicitConversions

/** A parser takes as input a sequence of tokens and outputs a program */
object Parser extends Parsers with Pipeline[Iterator[Token], Program] {
  // The parser takes tokens as input
  type Elem = Token

  def id: Parser[String] = accept(
    "identifier",
    { case IdToken(name) =>
      name
    }
  )

  implicit def delim(str: String): Parser[String] = accept(
    "delimiter",
    { case DelimToken(`str`) =>
      str
    }
  )

  def kw(str: String): Parser[String] = accept(
    "keyword",
    { case KwToken(`str`) =>
      str
    }
  )

  def program: Parser[Program] = positioned {
    rep(definition <~ ";") ~ rep(expr <~ ";") <~ accept(EOFToken()) ^^ {
      case defs ~ exprs =>
        Program(defs, exprs)
    }
  }

  def definition: Parser[Def] = positioned {
    kw("let") ~> id ~ opt("(" ~> rep1sep(id, ",") <~ ")") ~ opt(
      ":=" ~> expr
    ) ^^ {
      case id ~ None ~ bodyOpt         => Let(id, Seq(), bodyOpt)
      case id ~ Some(params) ~ bodyOpt => Let(id, params, bodyOpt)
    }
  }

  def prefixExpr: Parser[Expr] = positioned {
    opt(kw("not")) ~ simpleExpr ^^ {
      case Some(_) ~ e => Not(e)
      case None ~ e    => e
    }
  }

  def simpleExpr: Parser[Expr] = positioned {
    variableOrCall |
      kw("true") ^^^ True |
      kw("false") ^^^ False |
      "(" ~> expr <~ ")"
  }

  def variableOrCall: Parser[Expr] = positioned {
    id ~ opt("(" ~> rep1sep(expr, ",") <~ ")") ^^ {
      case id ~ None       => Apply(id, Seq())
      case id ~ Some(args) => Apply(id, args)
    }
  }

  def impliesExpr: Parser[Expr] = positioned {
    rep1sep(prefixExpr, kw("->")) ^^ { operands =>
      operands.reduceRight(Implies)
    }
  }

  def expr: Parser[Expr] = positioned {
    opt((kw("forall") | kw("exists")) ~ id <~ ".") ~ impliesExpr ^^ {
      case Some("forall" ~ x) ~ body => Forall(x, body)
      case Some(exists ~ x) ~ body   => Exists(x, body)
      case None ~ e                  => e
    }
  }

  protected def apply(tokens: Iterator[Token])(ctxt: Context): Program = {
    phrase(program)(new TokenReader(tokens.toSeq)) match {
      case Success(program, _) =>
        program

      case e: NoSuccess =>
        var rd = e.next
        while (!rd.atEnd) {
          println(rd.first)
          rd = rd.rest
        }
        println(e.next.first.pos2)
        println(e.next)
        ctxt.fatal(
          e.msg
        ) // TODO: when moving to Scallion, add the correct position
    }
  }
}
