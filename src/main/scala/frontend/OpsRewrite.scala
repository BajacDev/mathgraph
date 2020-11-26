package mathgraph.frontend
import mathgraph.util._
import mathgraph.frontend.{OpTrees => In, MGLTrees => Out}
import scala.util.Try

object OpsRewrite extends Pipeline[In.Program, Out.Program] {
  protected def apply(program: In.Program)(ctxt: Context): Out.Program = {

    // Defines the precedence and associativity of built-in operators
    val builtInOperators = Map("->" -> (true, 10))

    // Map from operator name to boolean indicating if right associative, and int indicating precedence
    val operators = program.defs.collect {
      case df @ In.OpLet(assoc, prec, _, op, _, _) =>
        val parsedAssoc =
          if (assoc == "left") false
          else if (assoc == "right") true
          else
            ctxt.fatal(
              "The associativity of an operator can only be 'left' or 'right'",
              df
            )

        val parsedPrec = Try(prec.toInt).getOrElse(
          ctxt.fatal(
            "The precedence of an operator must be a non-negative integer",
            df
          )
        )

        (op, (parsedAssoc, parsedPrec))
    }.toMap ++ builtInOperators

    // Rewrites the given sequence of operators parsing the associativities and precendences correctly
    def rewriteSequence(
        first: Out.Expr,
        opsAndExprs: Seq[(String, Position, Out.Expr)]
    ): Out.Expr = {
      // Retrieves the precedence of an operator or throws a fatal error if the operator does not exist
      def prec(op: (String, Position, Out.Expr)): Int = operators
        .get(op._1)
        .getOrElse(ctxt.fatal(s"Undeclared operator '$op._1'", op._2))
        ._2

      // Retrieves the minimum level of what follows the given operator
      def nextLevel(op: String): Int = {
        val (rightAssoc, prec) = operators(op)
        if (rightAssoc) prec else prec + 1
      }

      // Glues two expressions together using an operator
      def glueExprs(lhs: Out.Expr, op: String, rhs: Out.Expr): Out.Expr =
        if (op == "->") Out.Implies(lhs, rhs)
        else Out.Apply(op, Seq(lhs, rhs))

      // Uses precedence parsing to parse the linear sequence of operators
      def rec(
          level: Int,
          first: Out.Expr,
          opsAndExprs: Seq[(String, Position, Out.Expr)]
      ): (Out.Expr, Seq[(String, Position, Out.Expr)]) = {
        var expr = first
        var remaining = opsAndExprs

        // We keep gluing expressions together as long as they associate stronger than the minimum required level
        while (!remaining.isEmpty && prec(remaining.head) >= level) {
          val (op, _, nextExpr) = remaining.head
          val (rhs, rest) = rec(nextLevel(op), nextExpr, remaining.tail)
          expr = glueExprs(expr, op, rhs)
          remaining = rest
        }

        (expr, remaining)
      }

      // We recurse with the minimum level of precedence
      rec(0, first, opsAndExprs)._1
    }

    def transformExpr(expr: In.Expr): Out.Expr = expr match {
      case In.True              => Out.True
      case In.False             => Out.False
      case In.Apply(id, args)   => Out.Apply(id, args.map(transformExpr(_)))
      case In.Implies(lhs, rhs) =>
        // This case should never happen but it doesn't hurt treating it
        Out.Implies(
          transformExpr(lhs),
          transformExpr(rhs)
        )
      case In.Forall(ids, body) => Out.Forall(ids, transformExpr(body))
      case In.OpSequence(first, opsAndExprs) =>
        val mapped = opsAndExprs.map { case (op, pos, expr) =>
          (op, pos, transformExpr(expr))
        }
        rewriteSequence(transformExpr(first), mapped)
    }

    val In.Program(defs, axioms) = program
    val newDefs = defs.map {
      case In.Let(name, vars, body) =>
        Out.Let(name, vars, body.map(transformExpr(_)))
      case In.OpLet(_, _, lhs, op, rhs, body) =>
        // We don't need the precedence and associativity information anymore
        Out.Let(
          op,
          Seq(lhs, rhs),
          body.map(transformExpr(_))
        )
    }
    val newAxioms = axioms.map(transformExpr(_))
    Out.Program(newDefs, newAxioms)
  }
}
