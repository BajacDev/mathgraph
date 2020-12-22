package mathgraph.frontend.mgl
import mathgraph.util._
import mathgraph.frontend.{OpTrees => In, BackendTrees => Out, _}
import scala.util.Try
import scala.collection.mutable.Map

object NameAnalyzer extends Pipeline[In.Program, Out.Program] {
  protected def apply(program: In.Program)(ctxt: Context): Out.Program = {

    // Those are the tables that store the symbols and operators in the program
    // name -> (rightAssociative, precedence)
    val operators = Map[String, (Boolean, Int)]("->" -> (true, 10))
    // name -> arity
    val symbols = Map.empty[String, Int]

    // Checks that the given name doens't shadow a previously defined variable or symbol
    def checkShadowing(
        name: String,
        pos: Position,
        variables: Set[String]
    ): Unit = {
      if (variables(name))
        ctxt.warning(
          s"Variable '$name' shadows another variable of the same name",
          pos
        )
      else if (symbols.contains(name) || operators.contains(name))
        ctxt.warning(
          s"Variable '$name' shadows another symbol of the same name",
          pos
        )
    }

    // Transforms a definition, freshening all names
    def transformDefinition(df: In.Definition): Out.Definition = {
      // We check that it isn't a redefinition
      if (symbols.contains(df.name) || operators.contains(df.name))
        ctxt.error(s"Redefinition of symbol '${df.name}'", df)

      // We check that each parameter is only defined once and warn about shadowing
      df.params.groupBy(identity(_)).foreach { case (name, group) =>
        if (group.size > 1)
          ctxt.error(s"Parameter '$name' is declared several times", df)

        checkShadowing(name, df.pos, Set.empty)
      }

      // We transform the body of the definition
      Out.Let(
        df.name,
        df.params,
        df.body.map(transformExpr(_)(df.params.toSet))
      )
    }

    // Transforms an expression, given a map from names to identifiers
    def transformExpr(
        e: In.Expr
    )(implicit variables: Set[String]): Out.Expr = e match {
      // Trivial cases
      case In.True  => Out.True
      case In.False => Out.False

      // Variables / Constant symbols
      case In.Apply(name, Seq()) =>
        if (!variables(name)) {
          /*if (!symbols.contains(name))
            ctxt.fatal(s"Constant or variable '$name' not found", e)
          else if (symbols(name) != 0)
            ctxt.fatal(s"Expected arguments for function '$name'", e)*/
        }

        Out.Apply(name, Seq())

      // Function symbols
      case In.Apply(name, args) =>
        /*if (!symbols.contains(name) && !operators.contains(name))
          ctxt.fatal(s"Function '$name' not found", e)

        val expectedArity = if (symbols.contains(name)) symbols(name) else 2
        if (args.size != expectedArity)
          ctxt.fatal(
            s"Function '$name' expects $expectedArity arguments, but ${args.size} were given",
            e
          )*/

        Out.Apply(name, args.map(transformExpr))

      // This case should never happen but it doesn't hurt treating it
      case In.Implies(lhs, rhs) =>
        Out.Implies(transformExpr(lhs), transformExpr(rhs))

      // Forall introduces new variables in scope
      case In.Forall(names, body) =>
        // We check name uniqueness and shadowing
        names.groupBy(identity(_)).foreach { case (name, group) =>
          if (group.size > 1)
            ctxt.error(s"Variable '$name' is quantified several times", e)

          checkShadowing(name, e.pos, variables)
        }

        Out.Forall(names, transformExpr(body)(variables ++ names))

      // Operator sequences are parsed correctly
      case In.OpSequence(first, opsAndExprs) =>
        val mapped = opsAndExprs.map { case (op, pos, expr) =>
          (op, pos, transformExpr(expr))
        }
        rewriteSequence(transformExpr(first), mapped)
    }

    // Rewrites the given sequence of operators parsing the associativities and precendences correctly
    def rewriteSequence(
        first: Out.Expr,
        opsAndExprs: Seq[(String, Position, Out.Expr)]
    ): Out.Expr = {
      // Retrieves the signature of an operator or throws a fatal error if the operator does not exist
      def signature(op: String, pos: Position): (Boolean, Int) =
        operators.get(op) match {
          case Some(sig) => sig
          case _ =>
            ctxt.fatal(
              s"Unknown operator '$op'. Make sure to define it before using it.",
              pos
            )
        }

      // Retrieves the precedence of the next operator
      def prec(head: (String, Position, Out.Expr)): Int =
        signature(head._1, head._2)._2

      // Glues two expressions together using the given operator
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
          val (op, pos, nextExpr) = remaining.head
          val (rightAssoc, prec) = signature(op, pos)
          val nextLevel = if (rightAssoc) prec else prec + 1
          val (rhs, rest) = rec(nextLevel, nextExpr, remaining.tail)
          expr = glueExprs(expr, op, rhs)
          remaining = rest
        }

        (expr, remaining)
      }

      // We recurse with the minimum level of precedence
      rec(0, first, opsAndExprs)._1
    }

    // STEP 2: Then, we transform all the definitions and add them to the symbol table
    val newDefs = program.defs.map {
      case df @ In.OpLet(assoc, prec, lhs, op, rhs, body) =>
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

        val newDef = transformDefinition(df)
        operators += op -> ((parsedAssoc, parsedPrec))
        newDef

      case df @ In.Let(name, vars, body) =>
        val newDef = transformDefinition(df)
        symbols += name -> vars.size
        newDef
    }

    // STEP 3: Finally, we transform the expressions of the program
    val newAxioms = program.axioms.map(transformExpr(_)(Set.empty))

    // We return the transformed program
    Out.Program(newDefs, newAxioms)
  }
}
