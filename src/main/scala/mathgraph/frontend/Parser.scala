package mathgraph.frontend
import mathgraph.util._
import mathgraph.frontend.Trees._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.io.Source

/** A parser takes as input the name of a file and outputs a program */
object Parser extends StandardTokenParsers with Pipeline[String, Program] {
  lexical.delimiters ++= List(".", ";", ",", "->", "(", ")", ":=")
  lexical.reserved ++= List("let", "forall", "exists", "not")

  def program: Parser[Program] = positioned {
    rep(definition) ~ rep(expr) ^^ {
      case defs ~ exprs => Program(defs, exprs)
    }
  }

  def definition: Parser[Def] = positioned {
    "let" ~> ident ~ opt("(" ~> rep1sep(ident, ",") <~ ")") ~ opt(":=" ~> expr) <~ ";" ^^ {
      case id ~ None ~ bodyOpt => Let(id, Seq(), bodyOpt)
      case id ~ Some(params) ~ bodyOpt => Let(id, params, bodyOpt)
    }
  }

  def prefixExpr: Parser[Expr] = positioned {
    opt("not") ~ simpleExpr ^^ {
      case Some(_) ~ e => Not(e)
      case None ~ e => e
    }
  }

  def simpleExpr: Parser[Expr] = positioned {
    variableOrCall |
    "true" ^^^ True |
    "false" ^^^ False |
    "(" ~> expr <~ ")"
  }

  def variableOrCall: Parser[Expr] = positioned {
    ident ~ opt("(" ~> rep1sep(expr, ",") <~ ")") ^^ {
      case id ~ None => Apply(id, Seq())
      case id ~ Some(args) => Apply(id, args)
    }
  }

  def impliesExpr: Parser[Expr] = positioned {
    rep1sep(prefixExpr, "->") ^^ {
      case operands => operands.reduceRight(Implies)
    }
  }

  def expr: Parser[Expr] = positioned {
    opt(("forall" | "exists") ~ ident <~ ".") ~ impliesExpr ^^ {
      case Some("forall" ~ x) ~ body => Forall(x, body)
      case Some(exists ~ x) ~ body => Exists(x, body)
      case None ~ e => e
    }
  }

  protected def apply(file: String)(ctxt: Context): Program = {
    val fileContent = Source.fromFile(file).mkString
    val tokens = new lexical.Scanner(fileContent)

    phrase(program)(tokens) match {
      case Success(trees, _) =>
        println(trees)
        trees

      case e: NoSuccess =>
        ctxt.fatal(e.msg, e.next.pos)
    }
  }
}