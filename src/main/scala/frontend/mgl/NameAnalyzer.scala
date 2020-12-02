package mathgraph.frontend.mgl
import mathgraph.util._
import mathgraph.frontend.{OpTrees => In, MGLTrees => Out}
import scala.util.Try

object NameAnalyzer extends Pipeline[In.Program, (Out.Program, SymbolTable)] {
  protected def apply(program: In.Program)(ctxt: Context): Out.Program = {

    // This is the symbol table that will be progressively filled when discovering symbols
    val table = new SymbolTable

    // Defines the precedence and associativity of built-in operators
    val builtInOperators = Map("->" -> (true, 10))

    // STEP 1: We first add all the built-in operators to the symbol table
    builtInOperators.foreach { case (name, (rightAssoc, prec)) =>
      table.addOperator(name, rightAssoc, pred)
    }

    // Checks that the given name doens't shadow a previously defined variable or symbol
    def checkShadowing(name: String, pos: Position, variables: Map[String, Identifier]): Unit = {
      if (variables.contains(name))
        ctxt.warning(s"Variable '$name' shadows another variable of the same name", pos)
      else if (table.getSymbol(name).isDefined)
        ctxt.warning(s"Variable '$name' shadows another symbol of the same name", pos)
    }

    // Transforms a definition, freshening all names
    def transformDefinition(def: In.Definition, id: => Identifier): Out.Definition = {
      // We check that it isn't a redefinition
      if (table.getSymbol(def.name).isDefined)
        ctxt.error(s"Redefinition of symbol '${def.name}'", def)

      // We check that each parameter is only defined once and warn about shadowing
      def.params.groupBy(identity).foreach { case (name, group) =>
        if (group.size > 1)
          ctxt.error(s"Parameter '$name' is declared several times", df)

        checkShadowing(name, def.pos, Map.empty)
      }

      // We create fresh identifiers for the parameters
      val freshParams = defs.params.map(Identifier.fresh(_))
      val paramMap = def.params.zip(freshParams).toMap
      val newBody = body.map(transformExpr(_)(paramMap))

      // We only add the definition to the symbol table now, through the call by name parameter 'id'
      Out.Let(id, freshParams, newBody)
    }

    // Transforms an expression, given a map from names to identifiers
    def transformExpr(e: In.Expr)(implicit variables: Map[String, Identifier]): Out.Expr = e match {
      // Trivial cases
      case In.True => Out.True
      case In.False => Out.False

      // Variables / Constant symbols
      case In.Apply(name, Seq()) =>
        val id =
          if (variables.contains(name)) variables(name)
          else {
            val (id, sig) = table.getSymbol(name).getOrElse(ctxt.fatal(s"Constant or identifier '$name' not found", e))
            if (sig.arity != 0)
              ctxt.fatal(s"Expected arguments for function '$name'", e)
            id
          }

        Out.Apply(id, Seq())

      // Function symbols
      case In.Apply(name, args) =>
        val (id, sig) = table.getSymbol(name).getOrElse(ctxt.fatal(s"Function '$name' not found", e))
        if (sig.arity != args.size)
          ctxt.fatal(s"Function '$name' expects ${sig.arity} arguments, but ${args.size} were given", e)

        Out.Apply(id, args.map(transformExpr))

      // This case should never happen but it doesn't hurt treating it
      case In.Implies(lhs, rhs) =>
        Out.Implies(transformExpr(lhs), transformExpr(rhs))

      // Forall introduces new variables in scope
      case In.Forall(names, body) =>
        // We check name uniqueness and shadowing
        names.groupBy(identity).foreach { case (name, group) =>
          if (group.size > 1)
            ctxt.error(s"Variable '$name' is quantified several times", e)

          checkShadowing(name, e.pos, variables)
        }

        // Then, we create new names and recurse with the new names
        val freshNames = names.map(Identifier.fresh(_))
        Out.Forall(freshNames, transformExpr(body)(variables ++ names.zip(freshNames).toMap))

      // Operator sequences are parsed correctly
      case In.OpSequence(first, opsAndExprs) =>
        val mapped = opsAndExprs.map { case (op, pos, expr) => (op, pos, transformExpr(expr)) }
        rewriteSequence(transformExpr(first), mapped)
    }

    // Rewrites the given sequence of operators parsing the associativities and precendences correctly
    def rewriteSequence(
        first: Out.Expr,
        opsAndExprs: Seq[(String, Position, Out.Expr)]
    ): Out.Expr = {
      // Retrieves the signature of an operator or throws a fatal error if the operator does not exist
      def signature(op: String, pos: Position): (Identifier, OperatorSig) = table.getSymbol(op) match {
        case Some((id, sig: OperatorSig)) => (id, sig)
        case _ => ctxt.fatal(s"Undeclared operator '$op'", pos)
      }

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
          val (id, OperatorSig(rightAssoc, prec)) = signature(op, pos)
          val nextLevel = if (rightAssoc) prec else prec + 1
          val (rhs, rest) = rec(nextLevel, nextExpr, remaining.tail)
          expr = Out.Apply(id, Seq(expr, rhs))
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

        transformDefinition(df, table.addOperator(op, parsedAssoc, parsedPrec))

      case df @ In.Let(name, vars, body) =>
        transformDefinition(df, table.addSymbol(name, vars.size))
    }

    // STEP 3: Finally, we transform the expressions of the program
    val newAxioms = program.axioms.map(transformExpr(_)(Map.empty))

    // We return the new program, along with the symbol table
    (Out.Program(newDefs, newAxioms), table)
  }
}
