package mathgraph.frontend.tptp
import mathgraph.util._
import Tokens._
import mathgraph.frontend.TPTPTrees._
import mathgraph.frontend._
import Parser._
import java.io.File

object TPTPFrontend extends Pipeline[String, BackendTrees.Program] {

  // This is a dummy transformation from TPTP expressions to backend expressions
  // because they do not come from the same trait
  private def transformExpr(e: Expr): BackendTrees.Expr = e match {
    case True => BackendTrees.True
    case False => BackendTrees.False
    case Apply(name, args) => BackendTrees.Apply(name, args.map(transformExpr))
    case Implies(lhs, rhs) => BackendTrees.Implies(transformExpr(lhs), transformExpr(rhs))
    case Equals(lhs, rhs) => BackendTrees.Equals(transformExpr(lhs), transformExpr(rhs))
    case Forall(names, body) => BackendTrees.Forall(names, transformExpr(body))
  }

  def apply(file: String)(ctx: Context): BackendTrees.Program = {
    // Recursively resolves all the include statements from a given file until none are left
    def resolveIncludes(file: String, includes: Seq[Include]): Seq[Annotated] = {
      val parent = new File(file).getParent
      includes.flatMap {
        case Include(sub, filter) =>
          // We find the path of the file relative to the current file
          val includeFile = new File(parent, sub).getPath

          // We load that file
          val Program(newIncludes, newFormulas) = (Lexer andThen Parser).run(includeFile)(ctx)

          // And recursively resolve the includes
          val resolvedFormulas = resolveIncludes(includeFile, newIncludes)

          // Then, we filter the included formulas
          val filtered =
            if (filter.isEmpty) resolvedFormulas
            else resolvedFormulas.filter(f => filter.contains(f.name))

          filtered ++ newFormulas
      }
    }

    // We extract the TPTP program from the source
    val Program(includes, annotated) = (Lexer andThen Parser).run(file)(ctx)

    // Then, we resolve the includes to get all the formulas
    val formulas = resolveIncludes(file, includes) ++ annotated

    // We need to negate the conjecture to generate a program to test unsatisfiability
    val (axioms, conjectures) = formulas.partition {
      case _: Axiom => true
      case _: Conjecture => false
    }

    val newConjectures =
      if (conjectures.size > 1) ctx.fatal(s"A TPTP file cannot contain more than one conjecture.")
      else if (conjectures.isEmpty) {
        ctx.warning(s"No conjecture was found, this is probably a mistake.")
        Seq()
      } else {
        val Conjecture(_, conj) = conjectures(0)
        Seq(BackendTrees.Not(transformExpr(conj)))
      }

    val newAxioms = axioms.map {
      case Axiom(_, a) => transformExpr(a)
    }

    BackendTrees.Program(Seq(), newAxioms ++ newConjectures)
  }
}
