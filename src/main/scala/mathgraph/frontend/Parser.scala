package mathgraph.frontend
import mathgraph.util._
import mathgraph.frontend.Trees._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.io.Source

/** A parser takes as input the name of a file and outputs a program */
object Parser extends StandardTokenParsers with Pipeline[String, Program] {
  lexical.delimiters ++= List(".", ";", "->", "(", ")", "=")
  lexical.reserved ++= List("let", "forall", "exists")

  def program: Parser[Program] = positioned {
    rep(definition) ~ rep(expr) ^^ {
      case defs ~ exprs => Program(defs, exprs)
    }
  }

  def definition: Parser[Def] = positioned {
    "let" ^^^ Let("abc", Seq(), None)
  }

  def expr: Parser[Expr] = positioned {
    ident ^^ Symbol
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