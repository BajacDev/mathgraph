package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend.BackendTrees._
import scala.collection.mutable.Map

object NameAnalyzer extends Pipeline[Seq[Expr], Program] {
  def apply(exprs: Seq[Expr])(ctxt: Context): Program = {
    // Variables are uppercase in TPTP
    def isVariable(name: String): Boolean = name(0).isUpper

    // Map of the symbols name -> arity
    val symbols = Map.empty[String, Int]

    // Checks an expression, returning its free variables
    def transformExpr(e: Expr)(implicit variables: Set[String]): (Expr, Set[String]) = e match {
      case True => (True, Set.empty)
      case False => (False, Set.empty)

      // Variables or nullary symbol
      case Apply(name, Seq()) =>
        val isVar = isVariable(name)

        if (isVar && !variables(name))
          ctxt.error(s"Variable '$name' not found", e)
        
        if (!isVar) {
          if (!symbols.contains(name))
            symbols += name -> 0 // We register the symbols the first time we see them
          else if (symbols(name) != 0)
            ctxt.error(s"Inconsistent usage of symbol '$name'. It was previously given ${symbols{name}} arguments, but 0 were given.", e)
        }

        (Apply(name, Seq()), if (isVar) Set(name) else Set.empty)

      // Symbols
      case Apply(name, args) =>
        if (isVariable(name))
          ctxt.error(s"Illegal usage of variable '$name'", e)
        else if (!symbols.contains(name))
          symbols += name -> args.size
        else if (symbols(name) != args.size)
          ctxt.error(s"Inconsistent usage of symbol '$name'. It was previously given ${symbols{name}} arguments, but ${args.size} were given.", e)

        val (newArgs, freeVars) = args.map(transformExpr(_)).unzip
        (Apply(name, newArgs), freeVars.reduce(_ ++ _))

      case Implies(lhs, rhs) =>
        val (newLhs, lhsVars) = transformExpr(lhs)
        val (newRhs, rhsVars) = transformExpr(rhs)
        (Implies(newLhs, newRhs), lhsVars ++ rhsVars)

      case Equals(lhs, rhs) =>
        val (newLhs, lhsVars) = transformExpr(lhs)
        val (newRhs, rhsVars) = transformExpr(rhs)
        (Equals(newLhs, newRhs), lhsVars ++ rhsVars)

      case Forall(names, body) =>
        names.groupBy(identity(_)).foreach { case (name, group) =>
          if (group.size > 1)
            ctxt.error(s"Variable '$name' is quantified several times", e)

          if (variables(name))
            ctxt.warning(s"Variable '$name' shadows another variable of the same name", e)
        }

        val (newBody, freeVars) = transformExpr(body)
        (Forall(names, newBody), freeVars -- names)
    }

    val newDefs = symbols.map {
      case (name, arity) => Let(name, (1 to arity).map(i => s"x$i"), None)
    }.toSeq
    val newExprs = exprs.map { expr =>
      val (newExpr, freeVars) = transformExpr(expr)(Set.empty)
      Forall(freeVars.toSeq, newExpr) // We quantify over the free variables (in case of Clause Normal Form)
    }

    Program(newDefs, newExprs)
  }
}
