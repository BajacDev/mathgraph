package mathgraph.frontend
import scala.util.parsing.input.Positional
import scala.io

/** This object contains the definition of all the AST in mathgraph */
object Trees {
  abstract class Tree extends Positional
  abstract class Expr extends Tree
  abstract class Def extends Tree
  abstract class AbstractForall extends Expr

  type Identifier = String

  // ----------------------------------------------------
  // Programs
  // ----------------------------------------------------

  case class Program(defs: Seq[Def], axioms: Seq[Expr]) extends Tree

  // ----------------------------------------------------
  // Definitions
  // ----------------------------------------------------

  /** Representation of let in(x, S) or let inc(A, B) = ... */
  case class Let(name: Identifier, vars: Seq[Identifier], body: Option[Expr])
      extends Def

  // ----------------------------------------------------
  // Expressions
  // ----------------------------------------------------

  /** Representation of a -> b */
  case class Implies(lhs: Expr, rhs: Expr) extends Expr

  /** Representation of true */
  case object True extends Expr

  /** Representation of false */
  case object False extends Expr

  /** Representation of lhs(arg1, ..., argN) or lhs if there are no args */
  case class Apply(id: Identifier, args: Seq[Expr]) extends Expr

  /** Representation of forall */
  case class Forall(id: Identifier, body: Expr) extends AbstractForall

  /** Representation of forall with multiple free variable */
  case class MultiForall(ids: Seq[Identifier], body: Expr)
      extends AbstractForall

  // ----------------------------------------------------
  // Desugared expressions
  // ----------------------------------------------------

  /** Syntactic sugar for not */
  object Not {
    def apply(e: Expr): Expr = Implies(e, False)
  }

  /** Syntactic sugar for existential quantification */
  object Exists {
    def apply(id: Identifier, body: Expr): Expr = Not(Forall(id, Not(body)))
  }
}
