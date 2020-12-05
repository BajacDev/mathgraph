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
    def transformExpr(
        e: Expr
    )(implicit variables: Set[String]): (Expr, Set[String]) = e match {
      case True  => (True, Set.empty)
      case False => (False, Set.empty)

      // Variables or nullary symbol
      case Apply(name, Seq()) =>
        val isVar = isVariable(name)

        if (!isVar) {
          if (!symbols.contains(name))
            symbols += name -> 0 // We register the symbols the first time we see them
          else if (symbols(name) != 0)
            ctxt.error(
              s"Inconsistent usage of symbol '$name'. It was previously given ${symbols { name }} arguments, but 0 were given.",
              e
            )
        }

        (Apply(name, Seq()), if (isVar) Set(name) else Set.empty)

      // Symbols
      case Apply(name, args) =>
        if (isVariable(name))
          ctxt.error(s"Illegal usage of variable '$name'", e)
        else if (!symbols.contains(name))
          symbols += name -> args.size
        else if (symbols(name) != args.size)
          ctxt.error(
            s"Inconsistent usage of symbol '$name'. It was previously given ${symbols { name }} arguments, but ${args.size} were given.",
            e
          )

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
            ctxt.warning(
              s"Variable '$name' shadows another variable of the same name",
              e
            )
        }

        val (newBody, freeVars) = transformExpr(body)(variables ++ names)
        (Forall(names, newBody), freeVars -- names)
    }

    // We first transform the expressions, quantifying over free variables
    val newExprs = exprs.map { expr =>
      val (newExpr, freeVars) = transformExpr(expr)(Set.empty)
      Forall(
        freeVars.toSeq,
        newExpr
      ) // We quantify over the free variables (in case of Clause Normal Form)
    }

    // We split the symbols into the normal ones, which don't have any interpretation,
    // and the ones that have a meaning (<=>, & and |)
    val (specialSymbols, normalSymbols) = symbols.partition { case (name, _) =>
      Set("<=>", "&", "|").contains(name)
    }

    // Helper to create the definition of a binary operator
    def mkDef(name: String)(body: (Expr, Expr) => Expr): Let =
      Let(name, Seq("x", "y"), Some(body(Apply("x", Seq()), Apply("y", Seq()))))

    // Those are the definitions of the special symbols
    val specialDefs = Map(
      "<=>" -> mkDef("<=>")((x, y) =>
        Apply("&", Seq(Implies(x, y), Implies(y, x)))
      ),
      "&" -> mkDef("&")((x, y) => Not(Implies(x, Not(y)))),
      "|" -> mkDef("|")((x, y) => Implies(Not(x), y))
    )

    // Those are the dependencies between the special symbols
    val dependencies = Map(
      "<=>" -> Seq("&", "<=>"),
      "&" -> Seq("&"),
      "|" -> Seq("|")
    )

    // We compute the set of special symbols that need to be defined
    val needed = specialSymbols.map(_._1).flatMap(dependencies).toSeq.distinct

    // We create definitions without body for the normal symbols
    val normalDefs = normalSymbols.map { case (name, arity) =>
      Let(name, (1 to arity).map(i => s"x$i"), None)
    }.toSeq

    Program(needed.map(specialDefs) ++ normalDefs, newExprs)
  }
}
