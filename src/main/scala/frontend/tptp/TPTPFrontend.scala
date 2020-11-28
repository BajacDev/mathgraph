package mathgraph.frontend.tptp
import mathgraph.util._
import Tokens._
import mathgraph.frontend.Trees._
import Parser._
import java.io.File

object TPTPFrontend extends Pipeline[AbstractSource, Program] {

  val pipeline = Lexer andThen Parser

  def apply(source: AbstractSource)(ctx: Context): Program = {

    implicit val context = ctx

    val trees = pipeline.run(source)(ctx)

    val expressions = trees filterNot isInclude

    val imports = (trees filter isInclude) flatMap tptpImport

    Program(Seq(), expressions.map(toExpr) ++ imports.map(toExpr))
  }

  def isInclude(tree: Tree): Boolean = tree match {
    case TPTPInclude(_, _) => true
    case _                 => false
  }

  def toExpr(tree: Tree)(implicit ctx: Context): Expr = tree match {
    case Let(_, _, expr) if expr.isDefined => expr.get
    case _                                 => ctx.fatal(s"Unexpected tree during parsing: $tree", tree)
  }

  def tptpImport(tree: Tree)(implicit ctx: Context): Seq[Tree] = {

    val TPTPInclude(filename, toImport) = tree

    val externalTrees = pipeline.run(FileSource(s"example/${filename}"))(ctx)

    if (toImport.isEmpty)
      externalTrees filterNot isInclude

    toImport.map { case (formula, pos) =>
      externalTrees
        .find(matches(formula))
        .getOrElse(
          ctx.fatal(
            s"Unable to import formula ${formula} from ${filename}. Formula not found.",
            pos
          )
        )
    }
  }

  def matches(formula: String)(tree: Tree): Boolean = tree match {
    case Let(name, _, _) if name == formula => true
    case _                                  => false
  }
}
